#include <galaxy/GalaxyApi.h>
#include "gog.h"

//// GalaxyAllocator.h

void *gog_MakeAllocator(gog_GalaxyMalloc malloc, gog_GalaxyRealloc realloc, gog_GalaxyFree free){
  return (void *)new galaxy::api::GalaxyAllocator((galaxy::api::GalaxyMalloc)malloc, (galaxy::api::GalaxyRealloc)realloc, (galaxy::api::GalaxyFree)free);
}

void gog_FreeAllocator(void *allocator){
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

void *gog_MakeThreadFactory(gog_ThreadCreationFunction creator){
  return (void *)new ThreadFactory(creator);
}

void gog_FreeThreadFactor(void *factory){
  delete (ThreadFactory*)factory;
}

//// GalaxyApi.h

void gog_Init(struct gog_InitOptions *options){
  galaxy::api::InitOptions opts(options->client_id,
                                options->client_secret,
                                options->config_file_path,
                                (galaxy::api::GalaxyAllocator*)options->galaxy_allocator,
                                options->storage_path,
                                options->host,
                                options->port,
                                (ThreadFactory*)options->galaxy_thread_factory);
  galaxy::api::Init(opts);
}

void gog_Shutdown(){
  galaxy::api::Shutdown();
}

void gog_ShutdownEx(struct gog_ShutdownOptions *options){
  galaxy::api::ShutdownOptions opts(options->preserve_static_objects);
  galaxy::api::ShutdownEx(opts);
}

gog_Interface gog_User(){
  return (gog_Interface)galaxy::api::User();
}

gog_Interface gog_Friends(){
  return (gog_Interface)galaxy::api::Friends();
}

gog_Interface gog_Chat(){
  return (gog_Interface)galaxy::api::Chat();
}

gog_Interface gog_Matchmaking(){
  return (gog_Interface)galaxy::api::Matchmaking();
}

gog_Interface gog_Networking(){
  return (gog_Interface)galaxy::api::Networking();
}

gog_Interface gog_Stats(){
  return (gog_Interface)galaxy::api::Stats();
}

gog_Interface gog_Utils(){
  return (gog_Interface)galaxy::api::Utils();
}

gog_Interface gog_Apps(){
  return (gog_Interface)galaxy::api::Apps();
}

gog_Interface gog_Storage(){
  return (gog_Interface)galaxy::api::Storage();
}

gog_Interface gog_CustomNetworking(){
  return (gog_Interface)galaxy::api::CustomNetworking();
}

gog_Interface gog_Logger(){
  return (gog_Interface)galaxy::api::Logger();
}

gog_Interface gog_Telemetry(){
  return (gog_Interface)galaxy::api::Telemetry();
}

gog_Interface gog_CloudStorage(){
  return (gog_Interface)galaxy::api::CloudStorage();
}

void gog_ProcessData(){
  galaxy::api::ProcessData();
}

//// Errors.h

const char* gog_IError_GetName(gog_Interface error){
  return ((galaxy::api::IError*)error)->GetName();
}

const char* gog_IError_GetMsg(gog_Interface error){
  return ((galaxy::api::IError*)error)->GetMsg();
}

enum gog_ErrorType gog_IError_GetType(gog_Interface error){
  return (enum gog_ErrorType)((galaxy::api::IError*)error)->GetType();
}

gog_Interface gog_GetError(){
  return (gog_Interface)galaxy::api::GetError();
}

//// 

void gog_IListenerRegistrar_Register(gog_Interface registrar, enum gog_ListenerType type, gog_Interface listener){
  ((galaxy::api::IListenerRegistrar*)registrar)->Register((galaxy::api::ListenerType)type, (galaxy::api::IGalaxyListener *)listener);
}

void gog_IListenerRegistrar_Unregister(gog_Interface registrar, enum gog_ListenerType type, gog_Interface listener){
  ((galaxy::api::IListenerRegistrar*)registrar)->Unregister((galaxy::api::ListenerType)type, (galaxy::api::IGalaxyListener *)listener);
}

gog_Interface gog_ListenerRegistrar(){
  return (gog_Interface) galaxy::api::ListenerRegistrar();
}

gog_Interface gog_GameServerListenerRegistrar(){
  return (gog_Interface) galaxy::api::GameServerListenerRegistrar();
}

//// Generator is too dumb for these

void gog_ICloudStorage_GetFile_Callback(gog_Interface cloudstorage, const char* container, const char* name, void* userParam, gog_WriteFunc writeFunc, gog_Interface listener){
  ((galaxy::api::ICloudStorage*)cloudstorage)->GetFile(container, name, userParam, (galaxy::api::ICloudStorage::WriteFunc)writeFunc, (galaxy::api::ICloudStorageGetFileListener*)listener);
}

void gog_ICloudStorage_GetFile(gog_Interface cloudstorage, const char* container, const char* name, void* dataBuffer, uint32_t bufferLength, gog_Interface listener){
  ((galaxy::api::ICloudStorage*)cloudstorage)->GetFile(container, name, dataBuffer, bufferLength, (galaxy::api::ICloudStorageGetFileListener*)listener);
}

void gog_ICloudStorage_PutFile_Callback(gog_Interface cloudstorage, const char* container, const char* name, void* userParam, gog_ReadFunc readFunc, gog_RewindFunc rewindFunc, gog_Interface listener, enum gog_SavegameType savegameType, uint32_t timeStamp,const char* hash){
  ((galaxy::api::ICloudStorage*)cloudstorage)->PutFile(container, name, userParam, (galaxy::api::ICloudStorage::ReadFunc)readFunc, (galaxy::api::ICloudStorage::RewindFunc)rewindFunc, (galaxy::api::ICloudStoragePutFileListener*)listener, (galaxy::api::SavegameType)savegameType, timeStamp, hash);
}

void gog_ICloudStorage_PutFile(gog_Interface cloudstorage, const char* container, const char* name, const void* buffer, uint32_t bufferLength, gog_Interface listener, enum gog_SavegameType savegameType, uint32_t timeStamp, const char* hash){
  ((galaxy::api::ICloudStorage*)cloudstorage)->PutFile(container, name, buffer, bufferLength, (galaxy::api::ICloudStoragePutFileListener*)listener, (galaxy::api::SavegameType)savegameType, timeStamp, hash);
}

void gog_ICloudStorage_CalculateHash_Callback(gog_Interface cloudstorage, void* userParam, gog_ReadFunc readFunc, gog_RewindFunc rewindFunc, char* hashBuffer, uint32_t hashBufferSize){
  ((galaxy::api::ICloudStorage*)cloudstorage)->CalculateHash(userParam, (galaxy::api::ICloudStorage::ReadFunc)readFunc, (galaxy::api::ICloudStorage::RewindFunc)rewindFunc, hashBuffer, hashBufferSize);
}

void gog_ICloudStorage_CalculateHash(gog_Interface cloudstorage, const void* buffer, uint32_t bufferLength, char* hashBuffer, uint32_t hashBufferSize){
  ((galaxy::api::ICloudStorage*)cloudstorage)->CalculateHash(buffer, bufferLength, hashBuffer, hashBufferSize);
}

