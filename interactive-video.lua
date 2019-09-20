local utils = require "mp.utils"
local msg   = require "mp.msg"


--[[ Utility functions ]]------------------------------------------------------

-- Change this to `msg.info' to see debug messages on mpv output.
local msg_debug = msg.debug

-- Count elements in a table.
function count(tbl)
  local n = 0
  for _ in pairs(tbl) do n = n + 1 end
  return n
end

-- Make a copy a table (but not of its elements).
function shallow_copy(orig)
  local copy
  if type(orig) == "table" then
    copy = {}
    for key, val in pairs(orig) do
      copy[key] = val
    end
  else
    copy = orig
  end
  return copy
end

-- Round to nearest integer.
math.round = function(x)
  return math.floor(x + 0.5)
end


--[[ Time functions ]]---------------------------------------------------------

-- Only used for rounding purposes: having the exact value is not necessary.
-- Default to 25fps (i.e., 40ms).
local frame_duration = 40

-- Get frame duration from container-fps property.
function update_frame_duration()
  local fps = mp.get_property_number("container-fps") or 25
  frame_duration = 1000 / (fps > 0 and fps or 25)
end
mp.register_event("file-loaded", update_frame_duration)

-- Round time position (in ms) to a multiple of frame_duration.
function round_to_frame(time)
  return math.round(math.round(time / frame_duration) * frame_duration)
end

-- Time position of previous frame.
function prev_frame(time)
  return round_to_frame(time - frame_duration)
end

-- Format time position (in ms) as hh:mm:ss.msc.
function time_to_string(time)
  return string.format("%02d:%02d:%06.3f",
                       math.floor(time / 3600000),
                       math.floor(time /   60000) % 60,
                       (time % 60000) / 1000)
end

-- Retrieve the time-pos property (in ms) and round it to a frame position.
function get_time_pos()
  local t = (mp.get_property_number("time-pos") or 0) * 1000 - frame_duration
  return round_to_frame(t)
end

-- Seek playback to given time position (in ms).
function set_time_pos(time)
  msg_debug("seek to:        " .. time_to_string(time))
  local t = round_to_frame(time + frame_duration) / 1000
  mp.set_property_number("time-pos", t)
end

-- Retrieve the duration property (in ms) and round it to a frame position.
function get_duration()
  local t = (mp.get_property_number("duration") or 0) * 1000 - frame_duration
  return round_to_frame(t)
end


--[[ Global data and state variables ]]----------------------------------------

local initial_segment, segments, moments, preconditions, segment_groups
local state   = { active = false }


--[[ Apply impression ]]-------------------------------------------------------

function apply_impression(impression)
  if impression == nil then
    return

  elseif impression.type == "userState" then
    for var, val in pairs(impression.data.persistent or {}) do
      msg_debug("set variable:   " .. var .. " = " ..
                utils.to_string(val) .. " (was: " ..
                utils.to_string(state.vars[var]) .. ")")
      state.vars[var] = val
    end

  else
    msg.error("Invalid type of impression data: " ..
              utils.to_string(impression.type))
  end
end


--[[ Evaluate precondition ]]--------------------------------------------------

function eval_precondition(cond)
  if cond == nil then
    return true

  elseif type(cond) ~= "table" then
    return cond

  elseif #cond == 0 then
    msg.error("Empty precondition expression")

  elseif cond[1] == "not" and #cond == 2 then
    return not eval_precondition(cond[2])

  elseif cond[1] == "and" then
    for i = 2, #cond do
      if not eval_precondition(cond[i]) then return false end
    end
    return true

  elseif cond[1] == "or"  then
    for i = 2, #cond do
      if     eval_precondition(cond[i]) then return true  end
    end
    return false

  elseif cond[1] == "eql" and #cond == 3 then
    return eval_precondition(cond[2]) ==  eval_precondition(cond[3])

  elseif cond[1] == "persistentState" and #cond == 2 then
    return state.vars[cond[2]]

  else
    msg.error("Invalid precondition: " .. utils.to_string(cond))
  end

  return false
end


--[[ Format strings for displaying choices on OSD ]]---------------------------

function format_osd_choices(choices)
  local osd_choices = {}
  for i = 1, #choices do
    local str = ""
    for j, ch in ipairs(choices) do
      local b = i == j
      str = str .. "{\\fscx70\\fscy70\\an5"
                .. "\\pos(" .. ((2*j-1) / (2*#choices)) .. ",0.048)}"
                .. (i == j and "{\\c&HFFFFFF&}[ " or "{\\c&H7F7F7F&}")
                .. ch.text
                .. (i == j and               " ]" or               "")
                .. "\n"
    end
    table.insert(osd_choices, str)
  end
  return osd_choices
end


--[[ Update OSD according to current state ]]----------------------------------

function update_osd()
  -- Multiple-choice input.
  if state.osd_choices and type(state.osd_input) == "number" then
    mp.set_osd_ass(1, 1, state.osd_choices[state.osd_input])

  -- Code entry input.
  elseif state.osd_prompt and type(state.osd_input) == "string" then
    mp.set_osd_ass(1, 1, "{\\fscx70\\fscy70\\an4\\pos(0.02,0.048)}" ..
                         state.osd_prompt .. " " .. state.osd_input .. "_\n")

  -- Nothing to display.
  else
    mp.set_osd_ass(0, 0, "")
  end
end


--[[ Navigation and input control functions ]]---------------------------------

-- Dummy function.
function cmd_nop()
end

-- Seek forward.
function cmd_fwd(sec, skip)
  return function()
    process_events(get_time_pos() + sec * 1000, { no_skip = not skip,
                                                  ffwd    = true })
  end
end

-- Seek backward.
function cmd_bwd(sec, skip)
  return function()
    rewind(sec * 1000, { no_skip = not skip })
  end
end

-- Select next choice.
function cmd_input_next()
  if state.osd_input < #state.osd_choices then
    state.osd_input = state.osd_input + 1
  else
    state.osd_input = 1
  end
  update_osd()
end

-- Select previous choice.
function cmd_input_prev()
  if state.osd_input > 1 then
    state.osd_input = state.osd_input - 1
  else
    state.osd_input = #state.osd_choices
  end
  update_osd()
end

-- Input character.
function cmd_input_char(char)
  return function()
    state.osd_input = state.osd_input .. char
    update_osd()
  end
end

-- Delete last input character.
function cmd_input_backspace()
  if string.len(state.osd_input) > 0 then
    state.osd_input = string.sub(state.osd_input, 1, -2)
    update_osd()
  end
end

-- Submit current input.
function cmd_input_submit()
  state.osd_choices = nil
  state.osd_prompt  = nil
  table.insert(state.events, 1, { time = get_time_pos(), type = "submit",
                                  mom_id = state.osd_mom_id })
  update_osd()
  update_controls()
end


--[[ Key mappings for control functions ]]-------------------------------------

-- Merge key mappings.
-- Mappings in map1 take precedence over those in map2.
function mapping_merge(map1, map2)
  for key, ctrl in pairs(map2) do
    map1[key] = map1[key] or ctrl
  end
end

-- Key mapping for navigation.
local nav_mapping = {
  ["RIGHT"]       = { "iv-nav-right",  cmd_fwd( 10),      {repeatable=true} },
  ["SHIFT+RIGHT"] = { "iv-nav-sright", cmd_fwd( 10,true), {repeatable=true} },
  ["LEFT"]        = { "iv-nav-left",   cmd_bwd( 10),      {repeatable=true} },
  ["SHIFT+LEFT"]  = { "iv-nav-sleft",  cmd_bwd( 10,true), {repeatable=true} },
  ["UP"]          = { "iv-nav-up",     cmd_fwd( 60),      {repeatable=true} },
  ["SHIFT+UP"]    = { "iv-nav-sup",    cmd_fwd( 60,true), {repeatable=true} },
  ["DOWN"]        = { "iv-nav-down",   cmd_bwd( 60),      {repeatable=true} },
  ["SHIFT+DOWN"]  = { "iv-nav-sdown",  cmd_bwd( 60,true), {repeatable=true} },
  ["PGUP"]        = { "iv-nav-pgup",   cmd_fwd(1/0),      {repeatable=true} },
  ["SHIFT+PGUP"]  = { "iv-nav-spgup",  cmd_fwd(600,true), {repeatable=true} },
  ["PGDWN"]       = { "iv-nav-pgdwn",  cmd_bwd(600),      {repeatable=true} },
  ["SHIFT+PGDWN"] = { "iv-nav-spgdwn", cmd_bwd(600,true), {repeatable=true} },
}

-- Control mapping for multiple-choice input.
local input_choice_mapping = {
  ["RIGHT"] = { "iv-input-right", cmd_input_next, { repeatable = true } },
  ["LEFT"]  = { "iv-input-left",  cmd_input_prev, { repeatable = true } },
  ["ENTER"] = { "iv-input-enter", cmd_input_submit },
}
for _, key in ipairs({ "UP", "DOWN", "PGUP", "PGDWN" }) do
  input_choice_mapping[key] = { "iv-nop-"..key, cmd_nop }
end
mapping_merge(input_choice_mapping, nav_mapping)

-- Control mapping for code entry input.
local input_code_mapping = {
  ["BS"]    = { "iv-input-bs",    cmd_input_backspace, { repeatable = true } },
  ["ENTER"] = { "iv-input-enter", cmd_input_submit    },
}
for _, key in ipairs({ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" }) do
  input_code_mapping[key]       = { "iv-input-"  ..key, cmd_input_char(key) }
  input_code_mapping["KP"..key] = { "iv-input-kp"..key, cmd_input_char(key) }
end
for _, key in ipairs({ "RIGHT", "LEFT", "UP", "DOWN", "PGUP", "PGDWN" }) do
  input_code_mapping[key]       = { "iv-nop-"    ..key, cmd_nop }
end
mapping_merge(input_code_mapping, nav_mapping)


--[[ Update controls according to current state ]]-----------------------------

local current_mapping

function update_controls()
  local new_mapping

  if state.active then
    -- Multiple-choice input.
    if state.osd_choices and type(state.osd_input) == "number" then
      new_mapping = input_choice_mapping

    -- Code entry input.
    elseif state.osd_prompt and type(state.osd_input) == "string" then
      new_mapping = input_code_mapping

    -- No input: only navigation controls.
    else
      new_mapping = nav_mapping
    end
  end

  if new_mapping ~= current_mapping then
    -- Remove current control bindings.
    for key, ctrl in pairs(current_mapping or {}) do
      mp.remove_key_binding(ctrl[1])
    end

    -- Register new control bindings.
    current_mapping = new_mapping
    for key, ctrl in pairs(current_mapping or {}) do
      mp.add_forced_key_binding(key, table.unpack(ctrl))
    end
  end
end


--[[ Deactivate interactive video playback ]]----------------------------------

function deactivate()
  mp.unregister_event(on_tick)
  state   = { active = false }
  update_osd()
  update_controls()
end


--[[ Retrieve first valid segment from segment group ]]------------------------

function resolve_segment_group(group_id)
  for _, seg_id in ipairs(segment_groups[group_id] or {}) do
    if type(seg_id) == "table" and seg_id.segmentGroup then
      seg_id = resolve_segment_group(seg_id.segmentGroup)
      if seg_id then return seg_id end
    else
      local cond_id = seg_id
      if type(seg_id) == "table" then
        cond_id = seg_id.precondition
        seg_id  = seg_id.segment
      end
      if eval_precondition(preconditions[cond_id]) then
        return seg_id
      end
    end
  end
  return nil
end


--[[ Load segment and schedule all related events in the event queue ]]--------

function load_segment(seg_id)
  msg_debug("load segment:   " .. seg_id)
  state.seg_id = seg_id

  state.events = {}

  -- Add moment-related events.
  for i, mom in ipairs(moments[state.seg_id] or {}) do
    table.insert(state.events, { time = mom.startMs,
                                 type = "start", mom_id = i })
    table.insert(state.events, { time = prev_frame(mom.endMs),
                                 type = "end",   mom_id = i })
    if string.sub(mom.type, 1, 6) == "scene:" then
      table.insert(state.events, { time = mom.uiDisplayMS,
                                   type = "display", mom_id = i })
      table.insert(state.events, { time = prev_frame(mom.uiHideMS),
                                   type = "hide",    mom_id = i })
    end
  end

  -- Sort moment-related events.
  table.sort(state.events, function(ev1, ev2)
    if ev1.time ~= ev2.time or ev1.mom_id ~= ev2.mom_id then
      return ev1.time < ev2.time
    elseif ev1.type ~= ev2.type then
      return ev1.type == "start" or ev2.type == "end" or
            (ev1.type == "display" and ev2.type == "hide")
    else
      return false
    end
  end)

  -- Add segment start and end events.
  local seg = segments[state.seg_id]
  table.insert(state.events, 1, { time = seg.startTimeMs,
                                  type = "start_seg" })
  table.insert(state.events,    { time = prev_frame(seg.endTimeMs or
                                                    get_duration()),
                                  type = "end_seg"   })
end


--[[ Process all pending events until given (or current) time position ]]------

function process_events(time, flags)
  time  = time  or get_time_pos()
  flags = flags or {}

  local seek = flags.ffwd

  if not state.events then return end
  while #state.events > 0 and time >= state.events[1].time do
    -- Pop first event from queue.
    local ev = state.events[1]
    table.remove(state.events, 1)

    msg_debug("process event:  " .. time_to_string(ev.time) .. " " ..
              string.format("%-10s", ev.type) ..
              state.seg_id .. (ev.mom_id and ("/" .. ev.mom_id) or "") ..
              ((ev.type == "submit" or ev.type == "end") and state.osd_input
               and (" (" .. utils.to_string(state.osd_input) .. ")") or ""))

    local next_seg_id = nil

    -- Moment-related event?
    if ev.mom_id then
      local mom = moments[state.seg_id][ev.mom_id]

      -- Start of new moment?
      if ev.type == "start" then
        -- Check precondition, and remove all moments related to this event
        -- if it is not satisfied.
        if not eval_precondition(mom.precondition) then
          for i = #state.events, 1, -1 do
            if state.events[i].mom_id == ev.mom_id then
              table.remove(state.events, i)
            end
          end
        else
          apply_impression(mom.impressionData)
        end

      -- Scene-related event?
      elseif string.sub(mom.type, 1, 6) == "scene:" then
        -- Display user input controls.
        if ev.type == "display" then
          state.osd_mom_id  = ev.mom_id
          if mom.config.hasMultipleChoiceInput then
            state.osd_prompt  = "Enter code:"
            state.osd_input   = ""
          else
            state.osd_choices = format_osd_choices(mom.choices)
            state.osd_input   = mom.defaultChoiceIndex + 1
          end
          if state.hist[state.hist_idx].inputs[ev.mom_id] then
            state.osd_input   = state.hist[state.hist_idx].inputs[ev.mom_id]
          end
          if flags.no_skip and time > ev.time then
            time = ev.time
            seek = true
          end

        -- Hide user input controls.
        elseif ev.type == "hide" then
          state.osd_choices = nil
          state.osd_prompt  = nil
          state.osd_mom_id  = nil

        -- Select branch according to user input.
        elseif ev.type == "end" or
              (ev.type == "submit" and
               not mom.config.disableImmediateSceneTransition) then
          -- Record input in history.
          -- Clear all forward history if input different from recorded one.
          local hist = state.hist[state.hist_idx]
          if state.osd_input ~= hist.inputs[ev.mom_id] then
             hist.inputs[ev.mom_id] = state.osd_input
             for i, mi in ipairs(moments[state.seg_id]) do
               if string.sub(mi.type, 1, 6) == "scene:" and
                  mi.startMs >= mom.endMs then
                 hist.inputs[i] = nil
               end
             end
             for i = #state.hist, state.hist_idx+1, -1 do
               state.hist[i] = nil
             end
          end

          -- Find corresponding choice.
          local choice
          if mom.config.hasMultipleChoiceInput then
            for _, ch in ipairs(mom.choices) do
              if not ch.code or state.osd_input == ch.code then
                choice = ch
                break
              end
            end
            if not choice then
              msg.error("No choice available for input '" .. state.osd_input ..
                        "' in moment " .. state.seg_id .. "/" .. ev.mom_id)
              deactivate()
              return
            end
          else
            choice = mom.choices[state.osd_input]
          end
          apply_impression(choice.impressionData)

          -- Select next segment accordingly.
          if not (mom.trackingInfo and
                  mom.trackingInfo.optionType == "fakeOption") then
            next_seg_id = choice.segmentId or resolve_segment_group(choice.sg)
            if not next_seg_id then
              msg.error("No segment for choice '" .. choice.id .. "' " ..
                        "of moment " .. state.seg_id .. "/" .. ev.mom_id)
              deactivate()
              return
            end
          end
          state.osd_input = nil
        end
      end

    -- Start of current segment?
    elseif ev.type == "start_seg" then
      state.hist_idx = state.hist_idx + 1
      if not state.hist[state.hist_idx] then
        state.hist[state.hist_idx] = { seg_id = state.seg_id,
                                       vars   = shallow_copy(state.vars),
                                       inputs = {} }
      end

    -- End of current segment?
    elseif ev.type == "end_seg" then
      next_seg_id = resolve_segment_group(state.seg_id) or
                    next(segments[state.seg_id].next)
      if not next_seg_id then
        msg.debug("No segment after " .. state.seg_id .. "; " ..
                  "assuming end of video")
        deactivate()
        mp.commandv("playlist-next", "force")
        return
      end
    end

    -- Load next segment?
    if next_seg_id then
      local  cur_seg = segments[state.seg_id]
      local next_seg = segments[next_seg_id]

      -- If next segment does not directly follow current segment,
      -- we need to jump.
      if next_seg.startTimeMs ~= cur_seg.endTimeMs then
        if flags.ffwd then
          time = next_seg.startTimeMs + time - prev_frame(cur_seg.endTimeMs)
        else
          time = next_seg.startTimeMs
        end
        seek   = true

      -- Also jump if skipping end of current segment.
      elseif time < next_seg.startTimeMs then
        time   = next_seg.startTimeMs
        seek   = true
      end

      load_segment(next_seg_id)
    end
  end

  -- Jump to current time position, if required.
  if seek then
    set_time_pos(time)
  end

  -- Update OSD and controls according to current state.
  update_osd()
  update_controls()
end


--[[ Seek backwards and rewind history ]]--------------------------------------

function rewind(delay, flags)
  flags = flags or {}

  if state.hist_idx == 0 then return end

  local time   = get_time_pos()
  local seg_id = state.hist[state.hist_idx].seg_id
  local first  = true

  -- Keep rewinding from segment to segment until delay is consumed.
  while delay > 0 and (first or state.hist_idx > 1) do
    if first then
      first          = false
    else
      state.hist_idx = state.hist_idx - 1
      seg_id         = state.hist[state.hist_idx].seg_id
      time           = segments[seg_id].endTimeMs
    end

    -- If in no-skip mode, find previous scene, if any.
    if flags.no_skip then
      local prev_mom
      for _, mom in ipairs(moments[seg_id]) do
        if string.sub(mom.type, 1, 6) == "scene:" and
           time - delay <= mom.uiDisplayMS and mom.uiDisplayMS < time and
           (not prev_mom or prev_mom.uiDisplayMS < mom.uiDisplayMS) then
          prev_mom = mom
        end
      end
      if prev_mom then
        time  = prev_mom.uiDisplayMS
        delay = 0
        break
      end
    end

    -- Rewind until start of current segment.
    delay = delay - time + segments[seg_id].startTimeMs
    time  = segments[seg_id].startTimeMs
  end

  -- Adjust actual time.
  if delay < 0 then
    time = time - delay
  end

  -- Load current segment and state variables from history.
  load_segment(seg_id)
  shallow_copy(state.vars, state.hist[state.hist_idx].vars)
  state.hist_idx    = state.hist_idx - 1
  state.osd_choices = nil
  state.osd_prompt  = nil

  -- Re-run current segment's history forward until actual time.
  process_events(time, { ffwd = true })
end


--[[ Load and check JSON data files, if any, on file load ]]-------------------

function on_start_file(_)
  deactivate()

  -- Look for JSON data files.

  local dir  = utils.split_path(mp.get_property("path"))
  local base = mp.get_property("filename/no-ext")
  local seg  = utils.join_path(dir, base .. ".seg.json")
  local ivm  = utils.join_path(dir, base .. ".ivm.json")
  if not (utils.file_info(seg) and utils.file_info(ivm)) then
    return
  end
  msg.info("Found JSON data files for interactive video playback:\n" ..
           "  seg: " .. seg .. "\n" ..
           "  ivm: " .. ivm)

  -- Read and parse JSON data files.

  local file
  file = io.open(seg, "r")
  if not file then
    msg.error("Cannot read from " .. seg)
    return
  end
  seg  = utils.parse_json(file:read("*a"))
  file:close()

  file = io.open(ivm, "r")
  if not file then
    msg.error("Cannot read from " .. ivm)
    return
  end
  ivm  = utils.parse_json(file:read("*a"))
  file:close()

  -- Initialize data from JSON structures.

  segments        = seg.segments
  initial_segment = seg.initialSegment
  local video_id  = "" .. (seg.viewableId or "")

  if not segments then
    msg.error(      "segments not found in JSON data files")
    return
  elseif not initial_segment then
    msg.error("initialSegment not found in JSON data files")
    return
  elseif not video_id then
    msg.error(    "viewableId not found in JSON data files")
    return
  end

  if ivm.videos and ivm.videos[video_id] and
     ivm.videos[video_id].interactiveVideoMoments then
    ivm = ivm.videos[video_id].interactiveVideoMoments.value
  else
    ivm = nil
  end
  if not ivm then
    msg.error("interactiveVideoMoments not found in JSON data files")
    return
  end

  moments        = ivm.momentsBySegment
  preconditions  = ivm.preconditions
  segment_groups = ivm.segmentGroups
  state.vars     = shallow_copy(ivm.stateHistory)

  if not moments then
    msg.error("momentsBySegment not found in JSON data files")
    return
  elseif not preconditions then
    msg.error(   "preconditions not found in JSON data files")
    return
  elseif not segment_groups then
    msg.error(   "segmentGroups not found in JSON data files")
    return
  elseif not state.vars then
    msg.error(    "stateHistory not found in JSON data files")
    return
  end

  -- Sanity checks.

  local fail = false

  -- Check segment history.
  if not (ivm.segmentHistory and #ivm.segmentHistory == 1 and
          ivm.segmentHistory[1] == initial_segment) then
    msg.error("Invalid segmentHistory" ..
              ": expected " .. utils.to_string({ initial_segment }) ..
              ", got "      .. utils.to_string(ivm.segmentHistory))
    fail = true
  end

  -- Check segments.
  for k, seg in pairs(segments) do
    if (seg.endTimeMs or 1/0) <= seg.startTimeMs then
      msg.error("Segment " .. k .. " ends before it starts")
      fail = true
    end
    local has_scene = false
    for i, mom in ipairs(moments[k] or {}) do
      if string.sub(mom.type, 1, 6) == "scene:" and
         not (mom.trackingInfo and
              mom.trackingInfo.optionType == "fakeOption") then
        has_scene = true
        break
      end
    end
    if count(seg.next) > 1 and not has_scene and not segment_groups[k] then
      msg.error("Segment " .. k .. " requires a branching moment " ..
                "or a segment group")
      fail = true
    end
  end

  -- Check moments.
  local n_moments = 0
  for k, ml in pairs(moments) do
    n_moments = n_moments + #ml
    for i, mi in ipairs(ml) do
      local ki = k .. "/" .. i

      if mi.endMs <= mi.startMs then
        msg.error("Moment " .. ki .. " ends before it starts")
        fail = true
      end
      if mi.startMs < segments[k].startTimeMs then
        msg.error("Moment " .. ki .. " starts before its segment does")
        fail = true
      end
      if mi.endMs   > (segments[k].endTimeMs or 1/0) then
        msg.error("Moment " .. ki ..    " ends after its segment does")
        fail = true
      end

      -- Scene moments.
      if string.sub(mi.type, 1, 6) == "scene:" then
        if not mi.choices then
          msg.error("Moment " .. ki .. " has no branching choices")
          fail = true
        end
        if not (mi.uiDisplayMS and mi.uiHideMS) then
          msg.error("Moment " .. ki .. " has no display interval")
          fail = true
        else
          if mi.uiHideMS <= mi.uiDisplayMS then
            msg.error("Moment " .. ki .. "'s display interval is empty")
            fail = true
          end
          if mi.uiDisplayMS < mi.startMs or mi.uiHideMS >= mi.endMs then
            msg.warn ("Moment " .. ki .. "'s display interval overflows " ..
                      "out of bounds")
          end
        end
        if mi.endMs < (segments[k].endTimeMs or 1/0) and
           not (mi.trackingInfo and
                mi.trackingInfo.optionType == "fakeOption") then
          msg.warn ("Moment " .. ki .. " ends before its segment does")
        end

        for j, mj in ipairs(ml) do
          local kj = k .. "/" .. j
          if i ~= j and mi.startMs <= mj.startMs and mj.startMs < mi.endMs then
            msg.error("Moment " .. kj .. " starts while " .. ki ..
                      " is active")
            fail = true
          end
        end

      -- Notification moments.
      elseif mi.type == "notification:playbackImpression" then
        if mi.choices then
          msg.error("Moment " .. ki .. " has branching choices")
          fail = true
        end
        if not mi.impressionData then
          msg.warn ("Moment " .. ki .. " has no impression data")
        end

      elseif mi.type == "notification:action" then
        if mi.choices then
          msg.error("Moment " .. ki .. " has branching choices")
          fail = true
        end

      else
        msg.error  ("Moment " .. ki .. " has invalid type: " .. mi.type)
        fail = true
      end
    end
  end

  if fail then
    deactivate()
    return
  end

  -- Display stats.

  msg.verbose("Loaded JSON data files:\n" ..
              "  " .. count(segments)       .. " segments\n" ..
              "  " .. n_moments             .. " interactive moments\n" ..
              "  " .. count(preconditions)  .. " preconditions\n" ..
              "  " .. count(segment_groups) .. " segment groups\n" ..
              "  " .. count(state.vars)     .. " state variables")

  state.active   = true
  state.hist_idx = 0
  state.hist     = {}
  update_controls()
end


--[[ Jump to initial segment at beginning of playback ]]-----------------------

function on_file_loaded(_)
  if not state.active then return end
  mp.register_event("tick", on_tick)
  load_segment(initial_segment)
  process_events(segments[initial_segment].startTimeMs, { ffwd = true })
end


--[[ Process queued events on each frame ]]------------------------------------

function on_tick(_)
  process_events()
end


--[[ Register script entry-point events ]]-------------------------------------

mp.register_event("start-file",  on_start_file)
mp.register_event("file-loaded", on_file_loaded)
