
#include  "FTGlyphContainer.h"

/* (cname, cargs, cxxname, cxxarg, cxxtype)
    FTGLglyph* cname cargs
    {
        cxxname *g = new cxxname cxxarg;
        if(g->Error())
        {
            delete g;
            return NULL;
        }
        FTGLglyph *ftgl = (FTGLglyph *)malloc(sizeof(FTGLglyph));
        ftgl->ptr = g;
        ftgl->type = cxxtype;
        return ftgl;
    }
 */

//  (const FTGlyph* const Glyph(const unsigned int characterCode) const;)

     FTGlyph* ftglglyph (const unsigned int characterCode)
    {
        Glyph* g = new Glyph (const unsigned int characterCode);
        if (g->Error())
        {
            delete g;
            return NULL;
        }
        FTGLglyph* ftgl = (FTGLglyph*) malloc(sizeof (FTGLglyph));
        ftgl->ptr = g;
        ftgl->type = FTGlyph;
        return ftgl;
    }
