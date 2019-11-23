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

var propAddr = AudioObjectPropertyAddress(
    mSelector: AudioObjectPropertySelector(kAudioDevicePropertyMute),
    mScope: AudioObjectPropertyScope(kAudioObjectPropertyScopeOutput),
    mElement: AudioObjectPropertyElement(kAudioObjectPropertyElementMaster))

var isMuted: uint32 = 0
var propSize = UInt32(MemoryLayout.size(ofValue: isMuted))

let status2 = AudioHardwareServiceGetPropertyData(defaultOutputDeviceID, &propAddr, 0, nil, &propSize, &isMuted)

if status2 != 0 { exit(-1) }

if isMuted != 0 {
  print(0);
} else {
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
}
