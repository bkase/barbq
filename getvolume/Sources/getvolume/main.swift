import AudioToolbox
import Darwin

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

if status1 != 0 { fputs("status1 failed \(status1)\n", stderr); exit(-1) }

var propAddr = AudioObjectPropertyAddress(
    mSelector: AudioObjectPropertySelector(kAudioDevicePropertyMute),
    mScope: AudioObjectPropertyScope(kAudioObjectPropertyScopeOutput),
    mElement: AudioObjectPropertyElement(kAudioObjectPropertyElementMaster))

var isMuted: uint32 = 0
var propSize = UInt32(MemoryLayout.size(ofValue: isMuted))

let status2 = AudioObjectGetPropertyData(defaultOutputDeviceID, &propAddr, 0, nil, &propSize, &isMuted)

if status2 != 0 { fputs("status2 failed \(status2)\n", stderr); exit(-1) }

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

if status3 != 0 { fputs("status3 failed \(status3)\n", stderr); exit(-1) }

print(String(Int(round(volume * 100))) + "," + String(isMuted != 0))
