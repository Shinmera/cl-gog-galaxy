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
  galaxy::api::Init((const galaxy::api::InitOptions&)options);
}

void gog_Shutdown(){
  galaxy::api::Shutdown();
}

void gog_ShutdownEx(struct gog_ShutdownOptions *options){
  galaxy::api::ShutdownEx((const galaxy::api::ShutdownOptions&)options);
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
