import AudioToolbox

var defaultOutputDeviceID = AudioDeviceID(0)
var defaultOutputDeviceIDSize = UInt32(MemoryLayout.size(ofValue: defaultOutputDeviceID))

var getDefaultOutputDevicePropertyAddress = AudioObjectPropertyAddress(
    mSelector: kAudioHardwarePropertyDefaultOutputDevice,
    mScope: kAudioObjectPropertyScopeGlobal,
    mElement: AudioObjectPropertyElement(kAudioObjectPropertyElementMaster))

let status1 = AudioObjectGetPropertyData(
    AudioObjectID(kAudioObjectSystemObject),
    &getDefaultOutputDevicePropertyAddress,
    0,
    nil,
    &defaultOutputDeviceIDSize,
    &defaultOutputDeviceID)

if status1 != 0 { exit(-1) }

var volume = Float32(0.0)
var volumeSize = UInt32(MemoryLayout.size(ofValue: volume))

var volumePropertyAddress = AudioObjectPropertyAddress(
    mSelector: kAudioHardwareServiceDeviceProperty_VirtualMasterVolume,
    mScope: kAudioDevicePropertyScopeOutput,
    mElement: kAudioObjectPropertyElementMaster)

let status3 = AudioObjectGetPropertyData(
    defaultOutputDeviceID,
    &volumePropertyAddress,
    0,
    nil,
    &volumeSize,
    &volume)

if status3 != 0 { exit(-1) }

print(Int(round(volume * 100)))

