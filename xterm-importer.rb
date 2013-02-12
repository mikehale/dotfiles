#!/usr/bin/env ruby

require 'builder'
require 'pp'


class KeyMapBuilder
  attr_reader :mappings

  NSAlphaShiftKeyMask = 1 << 16 # caps lock
  NSShiftKeyMask = 2 << 16
  NSControlKeyMask = 4 << 16
  NSAlternateKeyMask = 8 << 16
  NSCommandKeyMask = 16 << 16
  NSNumericPadKeyMask = 32 << 16
  NSHelpKeyMask = 64 << 16
  NSFunctionKeyMask = 128 << 16
  
  NSUpArrowFunctionKey = "0xF700"
  NSDownArrowFunctionKey = "0xF701"
  NSLeftArrowFunctionKey = "0xF702"
  NSRightArrowFunctionKey = "0xF703"
  NSF1FunctionKey  = "0xF704"
  NSF2FunctionKey  = "0xF705"
  NSF3FunctionKey  = "0xF706"
  NSF4FunctionKey  = "0xF707"
  NSF5FunctionKey  = "0xF708"
  NSF6FunctionKey  = "0xF709"
  NSF7FunctionKey  = "0xF70A"
  NSF8FunctionKey  = "0xF70B"
  NSF9FunctionKey  = "0xF70C"
  NSF10FunctionKey = "0xF70D"
  NSF11FunctionKey = "0xF70E"
  NSF12FunctionKey = "0xF70F"
  NSF14FunctionKey = "0xF711"
  NSF15FunctionKey = "0xF712"
  NSF16FunctionKey = "0xF713"
  NSF17FunctionKey = "0xF714"
  NSF18FunctionKey = "0xF715"
  NSF19FunctionKey = "0xF716"
  NSF20FunctionKey = "0xF717"
  NSF21FunctionKey = "0xF718"
  NSF22FunctionKey = "0xF719"
  NSF23FunctionKey = "0xF71A"
  NSF24FunctionKey = "0xF71B"
  NSF25FunctionKey = "0xF71C"
  NSF26FunctionKey = "0xF71D"
  NSF27FunctionKey = "0xF71E"
  NSF28FunctionKey = "0xF71F"
  NSF29FunctionKey = "0xF720"
  NSF30FunctionKey = "0xF721"
  NSF31FunctionKey = "0xF722"
  NSF32FunctionKey = "0xF723"
  NSF33FunctionKey = "0xF724"
  NSF34FunctionKey = "0xF725"
  NSF35FunctionKey = "0xF726"
  NSInsertFunctionKey = "0xF727"
  NSDeleteFunctionKey = "0xF728"
  NSHomeFunctionKey = "0xF729"
  NSBeginFunctionKey = "0xF72A"
  NSEndFunctionKey = "0xF72B"
  NSPageUpFunctionKey = "0xF72C"
  NSPageDownFunctionKey = "0xF72D"
  NSPrintScreenFunctionKey = "0xF72E"
  NSScrollLockFunctionKey = "0xF72F"
  NSPauseFunctionKey = "0xF730"
  NSSysReqFunctionKey = "0xF731"
  NSBreakFunctionKey = "0xF732"
  NSResetFunctionKey = "0xF733"
  NSStopFunctionKey = "0xF734"
  NSMenuFunctionKey = "0xF735"
  NSUserFunctionKey = "0xF736"
  NSSystemFunctionKey = "0xF737"
  NSPrintFunctionKey = "0xF738"
  NSClearLineFunctionKey = "0xF739"
  NSClearDisplayFunctionKey = "0xF73A"
  NSInsertLineFunctionKey = "0xF73B"
  NSDeleteLineFunctionKey = "0xF73C"
  NSInsertCharFunctionKey = "0xF73D"
  NSDeleteCharFunctionKey = "0xF73E"
  NSPrevFunctionKey = "0xF73F"
  NSNextFunctionKey = "0xF740"
  NSSelectFunctionKey = "0xF741"
  NSExecuteFunctionKey = "0xF742"
  NSUndoFunctionKey = "0xF743"
  NSRedoFunctionKey = "0xF744"
  NSFindFunctionKey = "0xF745"
  NSHelpFunctionKey = "0xF746"
  NSModeSwitchFunctionKey = "0xF747"

  def initialize(mappings={})
    @mappings = mappings
  end

  def self.from(file)
    mappings = File.read(file).scan(/define-key map\s+"(.+)"\s\[(.+)\]/)
    escape_mappings = Hash[mappings.select{|k,v| k =~ /^\\e/ }.map{|k,v| [k.gsub('\e', ''), v] }]
    new(escape_mappings)
  end

  def build
    Hash[
         mappings.map do |code, key_chord|
           if key_code = key_code(key_chord)
             # pp [key_chord, code, key_code]
             [code, key_code]
           end
         end.compact
        ]
  end

  def iterm2_map
    key_maps = build.map do |code, key_code|
      %(  <key>#{key_code.downcase}</key>
  <dict>
    <key>Action</key>
    <integer>10</integer>
    <key>Text</key>
    <string>#{code}</string>
  </dict>)
    end.join("\n")

    %(<key>Keyboard Map</key>
<dict>
#{key_maps}
</dict>
)
  end

  def key_code(key_chord)
    keys = key_chord.split("-")
    modifiers = []
    last_key = nil

    keys.map do |key|
      case key
      when "C"
        modifiers << NSControlKeyMask
      when "M"
        modifiers << NSAlternateKeyMask
      when "S"
        modifiers << NSShiftKeyMask
      when "next", "select", "print", "insert", "kp", "prior", "multiply", "add", "separator", "subtract", "divide", /^\?/
        return false
      when /^\d+$/
        last_key = key
      else
        if %w[up down left right].include?(key)
          key = "#{key.capitalize}Arrow"
        else
          key = key.capitalize
        end

        const = "NS#{key}FunctionKey"

        if self.class.const_defined?(const)
          last_key = self.class.const_get(const)
        else
          raise "unknown key: #{key}"
        end
      end
    end

    # not sure why this is needed
    unless modifiers.empty?
      modifiers << NSNumericPadKeyMask
    end
    
    modifier_mask = modifiers.inject(0){|m,e| m+= e; m}.to_s(16)
    "#{last_key}-0x#{modifier_mask}"
  end
end

puts KeyMapBuilder.from(DATA).iterm2_map

# TODO extract key map from infocmp or similar
# pp `infocmp`.split($/).reject{|l| l =~ /^#/}.join($/).scan(/(\w+=[^,]+),/)
# exit


# Extracted from xterm.el
__END__
(define-key map "\eOP" [f1])
(define-key map "\eOQ" [f2])
(define-key map "\eOR" [f3])
(define-key map "\eOS" [f4])
(define-key map "\e[15~" [f5])
(define-key map "\e[17~" [f6])
(define-key map "\e[18~" [f7])
(define-key map "\e[19~" [f8])
(define-key map "\e[20~" [f9])
(define-key map "\e[21~" [f10])
(define-key map "\e[23~" [f11])
(define-key map "\e[24~" [f12])

(define-key map "\eO2P" [S-f1])
(define-key map "\eO2Q" [S-f2])
(define-key map "\eO2R" [S-f3])
(define-key map "\eO2S" [S-f4])
(define-key map "\e[1;2P" [S-f1])
(define-key map "\e[1;2Q" [S-f2])
(define-key map "\e[1;2R" [S-f3])
(define-key map "\e[1;2S" [S-f4])
(define-key map "\e[15;2~" [S-f5])
(define-key map "\e[17;2~" [S-f6])
(define-key map "\e[18;2~" [S-f7])
(define-key map "\e[19;2~" [S-f8])
(define-key map "\e[20;2~" [S-f9])
(define-key map "\e[21;2~" [S-f10])
(define-key map "\e[23;2~" [S-f11])
(define-key map "\e[24;2~" [S-f12])

(define-key map "\eO5P" [C-f1])
(define-key map "\eO5Q" [C-f2])
(define-key map "\eO5R" [C-f3])
(define-key map "\eO5S" [C-f4])
(define-key map "\e[15;5~" [C-f5])
(define-key map "\e[17;5~" [C-f6])
(define-key map "\e[18;5~" [C-f7])
(define-key map "\e[19;5~" [C-f8])
(define-key map "\e[20;5~" [C-f9])
(define-key map "\e[21;5~" [C-f10])
(define-key map "\e[23;5~" [C-f11])
(define-key map "\e[24;5~" [C-f12])

(define-key map "\eO6P" [C-S-f1])
(define-key map "\eO6Q" [C-S-f2])
(define-key map "\eO6R" [C-S-f3])
(define-key map "\eO6S" [C-S-f4])
(define-key map "\e[15;6~" [C-S-f5])
(define-key map "\e[17;6~" [C-S-f6])
(define-key map "\e[18;6~" [C-S-f7])
(define-key map "\e[19;6~" [C-S-f8])
(define-key map "\e[20;6~" [C-S-f9])
(define-key map "\e[21;6~" [C-S-f10])
(define-key map "\e[23;6~" [C-S-f11])
(define-key map "\e[24;6~" [C-S-f12])

(define-key map "\eO3P" [M-f1])
(define-key map "\eO3Q" [M-f2])
(define-key map "\eO3R" [M-f3])
(define-key map "\eO3S" [M-f4])
(define-key map "\e[15;3~" [M-f5])
(define-key map "\e[17;3~" [M-f6])
(define-key map "\e[18;3~" [M-f7])
(define-key map "\e[19;3~" [M-f8])
(define-key map "\e[20;3~" [M-f9])
(define-key map "\e[21;3~" [M-f10])
(define-key map "\e[23;3~" [M-f11])
(define-key map "\e[24;3~" [M-f12])

(define-key map "\eO4P" [M-S-f1])
(define-key map "\eO4Q" [M-S-f2])
(define-key map "\eO4R" [M-S-f3])
(define-key map "\eO4S" [M-S-f4])
(define-key map "\e[15;4~" [M-S-f5])
(define-key map "\e[17;4~" [M-S-f6])
(define-key map "\e[18;4~" [M-S-f7])
(define-key map "\e[19;4~" [M-S-f8])
(define-key map "\e[20;4~" [M-S-f9])
(define-key map "\e[21;4~" [M-S-f10])
(define-key map "\e[23;4~" [M-S-f11])
(define-key map "\e[24;4~" [M-S-f12])

(define-key map "\eOA" [up])
(define-key map "\eOB" [down])
(define-key map "\eOC" [right])
(define-key map "\eOD" [left])
(define-key map "\eOF" [end])
(define-key map "\eOH" [home])

(define-key map "\e[1;2A" [S-up])
(define-key map "\e[1;2B" [S-down])
(define-key map "\e[1;2C" [S-right])
(define-key map "\e[1;2D" [S-left])
(define-key map "\e[1;2F" [S-end])
(define-key map "\e[1;2H" [S-home])

(define-key map "\e[1;4A" [M-S-up])
(define-key map "\e[1;4B" [M-S-down])
(define-key map "\e[1;4C" [M-S-right])
(define-key map "\e[1;4D" [M-S-left])
(define-key map "\e[1;4F" [M-S-end])
(define-key map "\e[1;4H" [M-S-home])

(define-key map "\e[1;5A" [C-up])
(define-key map "\e[1;5B" [C-down])
(define-key map "\e[1;5C" [C-right])
(define-key map "\e[1;5D" [C-left])
(define-key map "\e[1;5F" [C-end])
(define-key map "\e[1;5H" [C-home])

(define-key map "\e[1;6A" [C-S-up])
(define-key map "\e[1;6B" [C-S-down])
(define-key map "\e[1;6C" [C-S-right])
(define-key map "\e[1;6D" [C-S-left])
(define-key map "\e[1;6F" [C-S-end])
(define-key map "\e[1;6H" [C-S-home])

(define-key map "\e[1;7A" [C-M-up])
(define-key map "\e[1;7B" [C-M-down])
(define-key map "\e[1;7C" [C-M-right])
(define-key map "\e[1;7D" [C-M-left])
(define-key map "\e[1;7F" [C-M-end])
(define-key map "\e[1;7H" [C-M-home])

(define-key map "\e[1;8A" [C-M-S-up])
(define-key map "\e[1;8B" [C-M-S-down])
(define-key map "\e[1;8C" [C-M-S-right])
(define-key map "\e[1;8D" [C-M-S-left])
(define-key map "\e[1;8F" [C-M-S-end])
(define-key map "\e[1;8H" [C-M-S-home])

# Changed these based on experimentation in osx
(define-key map "\e[1;9A" [M-up])
(define-key map "\e[1;9B" [M-down])
(define-key map "\e[1;9C" [M-right])
(define-key map "\e[1;9D" [M-left])
(define-key map "\e[1;9F" [M-end])
(define-key map "\e[1;9H" [M-home])

(define-key map "\e[2~" [insert])
(define-key map "\e[3~" [delete])
(define-key map "\e[5~" [prior])
(define-key map "\e[6~" [next])

(define-key map "\e[2;2~" [S-insert])
(define-key map "\e[3;2~" [S-delete])
(define-key map "\e[5;2~" [S-prior])
(define-key map "\e[6;2~" [S-next])

(define-key map "\e[2;4~" [M-S-insert])
(define-key map "\e[3;4~" [M-S-delete])
(define-key map "\e[5;4~" [M-S-prior])
(define-key map "\e[6;4~" [M-S-next])

(define-key map "\e[2;5~" [C-insert])
(define-key map "\e[3;5~" [C-delete])
(define-key map "\e[5;5~" [C-prior])
(define-key map "\e[6;5~" [C-next])

(define-key map "\e[2;6~" [C-S-insert])
(define-key map "\e[3;6~" [C-S-delete])
(define-key map "\e[5;6~" [C-S-prior])
(define-key map "\e[6;6~" [C-S-next])

(define-key map "\e[2;7~" [C-M-insert])
(define-key map "\e[3;7~" [C-M-delete])
(define-key map "\e[5;7~" [C-M-prior])
(define-key map "\e[6;7~" [C-M-next])

(define-key map "\e[2;8~" [C-M-S-insert])
(define-key map "\e[3;8~" [C-M-S-delete])
(define-key map "\e[5;8~" [C-M-S-prior])
(define-key map "\e[6;8~" [C-M-S-next])

(define-key map "\e[2;3~" [M-insert])
(define-key map "\e[3;3~" [M-delete])
(define-key map "\e[5;3~" [M-prior])
(define-key map "\e[6;3~" [M-next])

(define-key map "\e[4~" [select])
(define-key map "\e[29~" [print])

(define-key map "\eOj" [kp-multiply])
(define-key map "\eOk" [kp-add])
(define-key map "\eOl" [kp-separator])
(define-key map "\eOm" [kp-subtract])
(define-key map "\eOo" [kp-divide])
(define-key map "\eOp" [kp-0])
(define-key map "\eOq" [kp-1])
(define-key map "\eOr" [kp-2])
(define-key map "\eOs" [kp-3])
(define-key map "\eOt" [kp-4])
(define-key map "\eOu" [kp-5])
(define-key map "\eOv" [kp-6])
(define-key map "\eOw" [kp-7])
(define-key map "\eOx" [kp-8])
(define-key map "\eOy" [kp-9])
