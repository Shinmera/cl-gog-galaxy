#include <stdint.h>
#include <stdbool.h>

// GalaxyAllocator.h
typedef void* (*gog_GalaxyMalloc)(uint32_t size, const char* typeName);
typedef void* (*gog_GalaxyRealloc)(void* ptr, uint32_t newSize, const char* typeName);
typedef void (*gog_GalaxyFree)(void* ptr);
void *gog_MakeAllocator(gog_GalaxyMalloc malloc, gog_GalaxyRealloc realloc, gog_GalaxyFree free);
void gog_FreeAllocator(void *allocator);

// GalaxyThread.h
struct gog_Thread{
  void (*join)();
  bool (*joinable)();
  void (*detach)();
  void *user;
};

typedef void (*gog_ThreadEntryFunction)(void *user);
typedef struct gog_Thread* (*gog_ThreadCreationFunction)(gog_ThreadEntryFunction, void *user);
void *gog_MakeThreadFactory(gog_ThreadCreationFunction creator);
void gog_FreeThreadFactor(void *factory);

// GalaxyID.h
enum gog_ID_Type{
  ID_TYPE_UNASSIGNED,
  ID_TYPE_LOBBY,
  ID_TYPE_USER
};

inline uint64_t gog_ID_FromRealID(enum gog_ID_Type type, uint64_t value){
  return (uint64_t)type << 56 | value;
}

inline uint64_t gog_ID_GetRealID(uint64_t id){
  return id & 0xFFFFFFFFFFFFFF;
}

inline enum gog_ID_Type gog_ID_GetType(uint64_t id){
  return id >> 56;
}

inline bool gog_ID_IsValid(uint64_t id){
  return id != 0;
}

// InitOptions.h
struct gog_InitOptions {
  char *client_id;
  char *client_secret;
  char *config_file_path;
  char *storage_path;
  void *galaxy_allocator;
  void *galaxy_thread_factory;
  char *host;
  uint16_t port;
};

// ShutdownOptions.h
struct gog_ShutdownOptions {
  bool preserve_static_objects;
};

// GalaxyApi.h
typedef void* gog_Interface;

void gog_Init(struct gog_InitOptions *options);
void gog_Shutdown();
void gog_ShutdownEx(struct gog_ShutdownOptions *options);
gog_Interface gog_User();
gog_Interface gog_Friends();
gog_Interface gog_Chat();
gog_Interface gog_Matchmaking();
gog_Interface gog_Networking();
gog_Interface gog_Stats();
gog_Interface gog_Utils();
gog_Interface gog_Apps();
gog_Interface gog_Storage();
gog_Interface gog_CustomNetworking();
gog_Interface gog_Logger();
gog_Interface gog_Telemetry();
gog_Interface gog_CloudStorage();
void gog_ProcessData();

// Errors.h
enum gog_ErrorType {
  GOG_UNAUTHORIZED_ACCESS,
  GOG_INVALID_ARGUMENT,
  GOG_INVALID_STATE,
  GOG_RUNTIME_ERROR
};

gog_Interface gog_GetError();
char* gog_IError_GetName(gog_Interface error);
char* gog_IError_GetMsg(gog_Interface error);
enum gog_ErrorType gog_IError_GetType(gog_Interface error);

