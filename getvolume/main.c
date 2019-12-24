#include <AudioToolbox/AudioToolbox.h>
#include <stdio.h>
#include <math.h>

AudioDeviceID getCurrentlySelectedDeviceID() {
    UInt32 propertySize;
    AudioDeviceID deviceID = kAudioDeviceUnknown;

    AudioObjectPropertyAddress pa;
    pa.mScope = kAudioObjectPropertyScopeGlobal;
    pa.mElement = kAudioObjectPropertyElementMaster;
    pa.mSelector = kAudioHardwarePropertyDefaultOutputDevice;

    // get the default output device
    propertySize = sizeof(deviceID);

    OSStatus err = AudioObjectGetPropertyData(kAudioObjectSystemObject, &pa, 0, NULL, &propertySize, &deviceID);
    if (err != noErr) {
      fprintf(stderr, "Failed to get default deviceid\n");
    }
    return deviceID;
}

UInt32 isMuted(AudioDeviceID deviceID) {
  UInt32 propertySize;

  AudioObjectPropertyAddress pa;
  pa.mSelector = kAudioDevicePropertyMute;
  pa.mScope = kAudioObjectPropertyScopeOutput;
  pa.mElement = kAudioObjectPropertyElementMaster;

  UInt32 isMuted = 0;
  propertySize = sizeof(isMuted);

  OSStatus err = AudioObjectGetPropertyData(deviceID, &pa, 0, NULL, &propertySize, &isMuted);
  if (err != noErr) {
      fprintf(stderr, "Failed to get mute data\n");
  }

  return isMuted;
}

void getDeviceVolume(AudioDeviceID deviceID, float *volume) {
    UInt32 propertySize = sizeof(float);

    AudioObjectPropertyAddress pa;
    pa.mSelector = kAudioHardwareServiceDeviceProperty_VirtualMasterVolume;
    pa.mScope = kAudioDevicePropertyScopeOutput;
    pa.mElement = kAudioObjectPropertyElementMaster;

    OSStatus err = AudioObjectGetPropertyData(
      deviceID,
      &pa,
      0,
      NULL,
      &propertySize,
      volume);
    if (err != noErr) {
      fprintf(stderr, "Failed to get volume data\n");
      return;
    }
}

int main() {
  AudioDeviceID deviceID = getCurrentlySelectedDeviceID();
  UInt32 mute = isMuted(deviceID);
  float volume = 100.0;
  getDeviceVolume(deviceID, &volume);
  printf("%d,%d\n", (int)round(volume*100.0), mute);
  return 0;
}

