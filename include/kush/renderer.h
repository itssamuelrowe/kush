// Saturday, June 06 2020

#ifndef STRING_TEMPLATE_H
#define STRING_TEMPLATE_H

#include <jtk/collection/list/ArrayList.h>
#include <jtk/collection/map/HashMap.h>

/**
 * The rendering engine is logic-less, meaning there are no for loops and if statements.
 * However, the directives are flexible enough to suffice most needs.
 *
 * The directives are divided into simple and compound directives. A compound
 * directive has an opening tag and a closing tag. Whereas, a simple directive
 * uses a standalone tag. All tags are enclosed within the angle brackets.
 *
 * ## Simple Directives
 *
 * ### Variable Directive
 * The variale directive is the simplest directive. You can use it to access
 * render the value stored in a variable.
 * The general form of a variable directive is shown below:
 * ```
 * <.identifier>
 * ```
 * Here, the identifier indicates the variable whose value you want to render.
 *
 * ### Context Directive
 * The context directive is a special directive which represents the current
 * value within a loop directive.
 * The general form of a context directive is shown below:
 * ```
 * <$>
 * ```
 *
 * ## Compound Directives
 *
 * ### Loop Directive
 * The loop directive allows you to iterate over a list.
 * The general form of a loop directive is shown below:
 * ```
 * <@identifier>
 * ...
 * </>
 * ```
 * Here, the identifier indicates the list you want to iterative over.
 * The `</>` construct is the closing tag and indicates the termination of
 * the compound directive. Whatever is in between the opening and the closing
 * tags will be rendered n times, where n is the number of entries in the list.
 * You can embed any directive within the body of the loop directive.
 *
 *
 * ## Conditional Directive
 * The conditional directive allows you to enable or disable a section
 * within your template.
 * The general form of a conditional directive is shown below:
 * ```
 * <?identifier>
 * ...
 * </>
 * If there is a variable with the specified identifier defined in the context,
 * then the block inside the directive is executed. Otherwise, the whole
 * directive is skipped.
 *
 * ## Invoking Other Templates
 *
 * Consider the following context:
 * ```
 * {
 *     "name": "Samuel"
 * }
 *```
 * What happens when you feed this context to the template shown below?
 * ```
 * Hi, <.name>!
 * ```
 * The renderer would produce the string "Hi, Samuel!".
 *
 * What would happen if you fed the following context?
 * ```
 * {
 *     "name": {
 *         "first_name": "Samuel",
 *         "last_name": "Rowe"
 *     }
 * }
 * ```
 * The renderer would generate an error saying "Cannot render a map. Could not
 * find a suitable template."
 *
 * Before we proceed, remember that the variable directive can render only primitive
 * values such as integers, decimals, booleans, and strings. But there are
 * legitimate cases where maps and lists should be rendered. We will talk about
 * rendering lists later. As for maps, this is where the ability of a template
 * to invoke other template comes in.
 *
 * So the idea is, we assign each template an optional identifier. When a map
 * is encountered in a variable directive, the renderer looks for a special key
 * called "$tag" within the map. The value associated with "$tag" tells the
 * renderer which template to use to render the map. The map finds a matching
 * template and then injects the map as the context.
 *
 * Consider the following template whose identifier is "name".
 * ```
 * {.first_name} {last_name}
 * ```
 *
 * Now consider another template whose identifier is "main".
 * ```
 * Hi, {.name}!
 * ```
 *
 * When we render the main template with the context shown below:
 * ```
 * {
 *     "name": {
 *         "first_name": "Samuel",
 *         "last_name": "Rowe"
 *         "$tag": "name"
 *     }
 * }
 * ```
 *
 * When the engine encounters `{.name}` and realizes that the name is a map,
 * it looks for the `$tag` key. If it is not present, it reports an error as
 * usual. Otherwise, it will retrieve the identifier and look for a template
 * with the same identifier. The renderer reports an error if a suitable
 * template was not found. Otherwise, it will invoke the second template with
 * the context `{ "first_name": "Samuel", "last_name": "Rowe" }`. The result
 * of rendering the second template is replaced with `<.name>`. So the renderer
 * returns the string `"Hi, Samuel Rowe!"`
 *
 * The ability to invoke other templates allows you to render recursive data
 * structures.
 *
 * Imagine you wanted to print a tree (each node here is simply a map). You
 * would have to create template with the identifier "node".
 * ``
 * <.title>
 * <.children>
 *     <$>
 * </>
 *
 * Now if you feed it:
 * {
 *     "title": "directory_a',
 *     "$tag": "node",
 *     children: [
 *         {
 *             "title": "directory_b",
 *             "$tag": "node",
 *             children: [
 *                 {
 *                     "title": "file_1",
 *                     "$tag": "node",
 *                     children: []
 *                 }
 *             ]
 *         },
 *         {
 *             "title": "directory_c",
 *             "$tag": "node",
 *             children: []
 *         }
 *     ]
 * }
 * ```
 * The renderer would generate the following output:
 * ```
 * directory_a
 *     directory_b
 *         file_1
 *     directory_c
 * ```
 *
 * As you can see, the best part is you do not have to do any extra work for
 * indentation. The renderer will automatically take care of indentation.
 * This becomes really important when you are using the renderer to generate
 * code that needs to be well formatted.
 */

#define TEMPLATE_ENTRY_STRING 0
#define TEMPLATE_ENTRY_INTEGER 1
#define TEMPLATE_ENTRY_DECIMAL 2
#define TEMPLATE_ENTRY_LIST 3
#define TEMPLATE_ENTRY_MAP 4
#define TEMPLATE_ENTRY_CUSTOM 5
#define TEMPLATE_ENTRY_BOOLEAN 6

typedef struct TemplateEntry TemplateEntry;

struct TemplateEntry {
    uint8_t tag;
    TemplateEntry* parent;
    union {
        int32_t integer;
        double decimal;
        struct {
            int32_t length;
            uint8_t* bytes;
        } string;
        jtk_ArrayList_t* list;
        jtk_HashMap_t* map;
        void* custom;
        bool boolean;
    };
};

TemplateEntry* makeIntegerEntry(TemplateEntry* parent, int32_t value);
TemplateEntry* makeDecimalEntry(TemplateEntry* parent, double value);
TemplateEntry* makeStringEntry(TemplateEntry* parent, uint8_t* bytes, int32_t length);
TemplateEntry* makeListEntry(TemplateEntry* parent);
TemplateEntry* makeMapEntry(TemplateEntry* parent);
TemplateEntry* makeCustomEntry(TemplateEntry* parent, void* custom);

typedef uint8_t* (*RenderCustom)(void* custom, int32_t* length);
typedef void (*DestroyCustom)(void* custom, int32_t* length);

struct StringRenderer {
    TemplateEntry* root;
    RenderCustom renderCustom;
    DestroyCustom destroyCustom;
};

typedef struct StringRenderer StringRenderer;

StringRenderer* stringRendererNew();
void stringRendererDelete(StringRenderer* self);
void stringRendererAdd_i(StringRenderer* self, const uint8_t* name, int32_t value);
void stringRendererAdd_d(StringRenderer* self, const uint8_t* name, double value);
void stringRendererAdd_s(StringRenderer* self, const uint8_t* name, const uint8_t* bytes);
void stringRendererAdd_v(StringRenderer* self, const uint8_t* name, void* custom);
void stringRendererAddEx_i(StringRenderer* self, const uint8_t* name, int32_t length, int32_t value);
void stringRendererAddEx_d(StringRenderer* self, const uint8_t* name, int32_t length, double value);
void stringRendererAddEx_s(StringRenderer* self, const uint8_t* name, int32_t length, const uint8_t* bytes, int32_t bytesSize);
void stringRendererAddEx_v(StringRenderer* self, const uint8_t* name, int32_t length, void* custom);

void stringRendererSet_i(StringRenderer* self, const uint8_t* name, int32_t value);
void stringRendererSet_d(StringRenderer* self, const uint8_t* name, double value);
void stringRendererSet_s(StringRenderer* self, const uint8_t* name, const uint8_t* bytes);
void stringRendererSet_v(StringRenderer* self, const uint8_t* name, void* custom);
void stringRendererSetEx_i(StringRenderer* self, const uint8_t* name, int32_t length, int32_t value);
void stringRendererSetEx_d(StringRenderer* self, const uint8_t* name, int32_t length, double value);
void stringRendererSetEx_s(StringRenderer* self, const uint8_t* name, int32_t length, const uint8_t* bytes, int32_t bytesSize);
void stringRendererSetEx_v(StringRenderer* self, const uint8_t* name, int32_t length, void* custom);

#endif /* STRING_TEMPLATE_H */