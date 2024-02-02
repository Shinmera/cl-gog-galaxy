#include <galaxy/GalaxyApi.h>
#include "gog.h"

//// GalaxyAllocator.h

GOG_EXPORT void *gog_MakeAllocator(gog_GalaxyMalloc malloc, gog_GalaxyRealloc realloc, gog_GalaxyFree free){
  return (void *)new galaxy::api::GalaxyAllocator((galaxy::api::GalaxyMalloc)malloc, (galaxy::api::GalaxyRealloc)realloc, (galaxy::api::GalaxyFree)free);
}

GOG_EXPORT void gog_FreeAllocator(void *allocator){
  delete (galaxy::api::GalaxyAllocator*)allocator;
}

//// GalaxyThread.h

class ThreadFactory : public galaxy::api::IGalaxyThreadFactory {
public:
  gog_ThreadCreationFunction creator;
  ThreadFactory(gog_ThreadCreationFunction creator) : creator(creator) {}

  galaxy::api::IGalaxyThread* SpawnThread(galaxy::api::ThreadEntryFunction const entryPoint, galaxy::api::ThreadEntryParam param) override {
    return (galaxy::api::IGalaxyThread*)this->creator(entryPoint, param);
  }
};

GOG_EXPORT void *gog_MakeThreadFactory(gog_ThreadCreationFunction creator){
  return (void *)new ThreadFactory(creator);
}

GOG_EXPORT void gog_FreeThreadFactor(void *factory){
  delete (ThreadFactory*)factory;
}

//// GalaxyApi.h

GOG_EXPORT void gog_Init(struct gog_InitOptions *options){
  galaxy::api::Init((const galaxy::api::InitOptions&)options);
}

GOG_EXPORT void gog_Shutdown(){
  galaxy::api::Shutdown();
}

GOG_EXPORT void gog_ShutdownEx(struct gog_ShutdownOptions *options){
  galaxy::api::ShutdownEx((const galaxy::api::ShutdownOptions&)options);
}

GOG_EXPORT gog_Interface gog_User(){
  return (gog_Interface)galaxy::api::User();
}

GOG_EXPORT gog_Interface gog_Friends(){
  return (gog_Interface)galaxy::api::Friends();
}

GOG_EXPORT gog_Interface gog_Chat(){
  return (gog_Interface)galaxy::api::Chat();
}

GOG_EXPORT gog_Interface gog_Matchmaking(){
  return (gog_Interface)galaxy::api::Matchmaking();
}

GOG_EXPORT gog_Interface gog_Networking(){
  return (gog_Interface)galaxy::api::Networking();
}

GOG_EXPORT gog_Interface gog_Stats(){
  return (gog_Interface)galaxy::api::Stats();
}

GOG_EXPORT gog_Interface gog_Utils(){
  return (gog_Interface)galaxy::api::Utils();
}

GOG_EXPORT gog_Interface gog_Apps(){
  return (gog_Interface)galaxy::api::Apps();
}

GOG_EXPORT gog_Interface gog_Storage(){
  return (gog_Interface)galaxy::api::Storage();
}

GOG_EXPORT gog_Interface gog_CustomNetworking(){
  return (gog_Interface)galaxy::api::CustomNetworking();
}

GOG_EXPORT gog_Interface gog_Logger(){
  return (gog_Interface)galaxy::api::Logger();
}

GOG_EXPORT gog_Interface gog_Telemetry(){
  return (gog_Interface)galaxy::api::Telemetry();
}

GOG_EXPORT gog_Interface gog_CloudStorage(){
  return (gog_Interface)galaxy::api::CloudStorage();
}

GOG_EXPORT void gog_ProcessData(){
  galaxy::api::ProcessData();
}

//// Errors.h

GOG_EXPORT const char* gog_IError_GetName(gog_Interface error){
  return ((galaxy::api::IError*)error)->GetName();
}

GOG_EXPORT const char* gog_IError_GetMsg(gog_Interface error){
  return ((galaxy::api::IError*)error)->GetMsg();
}

GOG_EXPORT enum gog_ErrorType gog_IError_GetType(gog_Interface error){
  return (enum gog_ErrorType)((galaxy::api::IError*)error)->GetType();
}

GOG_EXPORT gog_Interface gog_GetError(){
  return (gog_Interface)galaxy::api::GetError();
}

//// 

GOG_EXPORT void gog_IListenerRegistrar_Register(gog_Interface registrar, enum gog_ListenerType type, gog_Interface listener){
  ((galaxy::api::IListenerRegistrar*)registrar)->Register((galaxy::api::ListenerType)type, (galaxy::api::IGalaxyListener *)listener);
}

GOG_EXPORT void gog_IListenerRegistrar_Unregister(gog_Interface registrar, enum gog_ListenerType type, gog_Interface listener){
  ((galaxy::api::IListenerRegistrar*)registrar)->Unregister((galaxy::api::ListenerType)type, (galaxy::api::IGalaxyListener *)listener);
}

GOG_EXPORT gog_Interface gog_ListenerRegistrar(){
  return (gog_Interface) galaxy::api::ListenerRegistrar();
}

GOG_EXPORT gog_Interface gog_GameServerListenerRegistrar(){
  return (gog_Interface) galaxy::api::GameServerListenerRegistrar();
}

//// Generator is too dumb for these

GOG_EXPORT void gog_ICloudStorage_GetFile_Callback(gog_Interface cloudstorage, const char* container, const char* name, void* userParam, gog_WriteFunc writeFunc, gog_Interface listener){
  ((galaxy::api::ICloudStorage*)cloudstorage)->GetFile(container, name, userParam, (galaxy::api::ICloudStorage::WriteFunc)writeFunc, (galaxy::api::ICloudStorageGetFileListener*)listener);
}

GOG_EXPORT void gog_ICloudStorage_GetFile(gog_Interface cloudstorage, const char* container, const char* name, void* dataBuffer, uint32_t bufferLength, gog_Interface listener){
  ((galaxy::api::ICloudStorage*)cloudstorage)->GetFile(container, name, dataBuffer, bufferLength, (galaxy::api::ICloudStorageGetFileListener*)listener);
}

GOG_EXPORT void gog_ICloudStorage_PutFile_Callback(gog_Interface cloudstorage, const char* container, const char* name, void* userParam, gog_ReadFunc readFunc, gog_RewindFunc rewindFunc, gog_Interface listener, enum gog_SavegameType savegameType, uint32_t timeStamp,const char* hash){
  ((galaxy::api::ICloudStorage*)cloudstorage)->PutFile(container, name, userParam, (galaxy::api::ICloudStorage::ReadFunc)readFunc, (galaxy::api::ICloudStorage::RewindFunc)rewindFunc, (galaxy::api::ICloudStoragePutFileListener*)listener, (galaxy::api::SavegameType)savegameType, timeStamp, hash);
}

GOG_EXPORT void gog_ICloudStorage_PutFile(gog_Interface cloudstorage, const char* container, const char* name, const void* buffer, uint32_t bufferLength, gog_Interface listener, enum gog_SavegameType savegameType, uint32_t timeStamp, const char* hash){
  ((galaxy::api::ICloudStorage*)cloudstorage)->PutFile(container, name, buffer, bufferLength, (galaxy::api::ICloudStoragePutFileListener*)listener, (galaxy::api::SavegameType)savegameType, timeStamp, hash);
}

GOG_EXPORT void gog_ICloudStorage_CalculateHash_Callback(gog_Interface cloudstorage, void* userParam, gog_ReadFunc readFunc, gog_RewindFunc rewindFunc, char* hashBuffer, uint32_t hashBufferSize){
  ((galaxy::api::ICloudStorage*)cloudstorage)->CalculateHash(userParam, (galaxy::api::ICloudStorage::ReadFunc)readFunc, (galaxy::api::ICloudStorage::RewindFunc)rewindFunc, hashBuffer, hashBufferSize);
}

GOG_EXPORT void gog_ICloudStorage_CalculateHash(gog_Interface cloudstorage, const void* buffer, uint32_t bufferLength, char* hashBuffer, uint32_t hashBufferSize){
  ((galaxy::api::ICloudStorage*)cloudstorage)->CalculateHash(buffer, bufferLength, hashBuffer, hashBufferSize);
}

