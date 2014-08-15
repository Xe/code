#include "Modules.h"
#include "User.h"
#include "Chan.h"

class CDetachPart : public CModule {
public:
        MODCONSTRUCTOR(CDetachPart) {}

        virtual EModRet OnUserPart(CString& sChannel, CString& sMessage) {
                CChan* pChan = m_pUser->FindChan(sChannel);

                if (pChan && !pChan->IsDetached()) {
                        pChan->DetachUser();
                        return HALTCORE;
                }

                return CONTINUE;
        }
};

MODULEDEFS(CDetachPart, "Detach on part")
