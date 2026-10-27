/*
 * The codec the generator would emit for list_change_reason_categories_response
 * and the types it holds: one function a type and a format, calling yyjson and
 * msgpack-c directly, in the field order of the struct, with the wire shapes of
 * the reflectors (tenant_id and time_point as strings, enums by name).
 */
#include "codecs.hpp"
#include "ores.platform/time/datetime.hpp"
#include <msgpack.h>
#include <yyjson.h>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <stdexcept>
#include <string_view>

namespace bench::generated {

namespace {

using ores::dq::domain::change_reason_category;
using ores::platform::time::datetime;
using ores::utility::domain::field_failure;
using ores::utility::domain::outcome;
using ores::utility::domain::result;

constexpr std::array<std::string_view, 7> outcome_names{
    "ok", "invalid", "denied", "missing", "conflict", "unavailable", "failed"};

[[noreturn]] void fail(std::string_view what) {
    throw std::runtime_error("Failed to decode " + std::string(what) + ".");
}

outcome outcome_from(std::string_view s) {
    for (std::size_t i = 0; i < outcome_names.size(); ++i)
        if (outcome_names[i] == s)
            return static_cast<outcome>(i);
    fail("outcome");
}

/*
 * JSON. Strings the value owns go into the document without a copy; strings
 * made here for the wire are copied, since they die before the write.
 */
yyjson_mut_val* str(yyjson_mut_doc* d, const std::string& s) {
    return yyjson_mut_strn(d, s.data(), s.size());
}

yyjson_mut_val* owned(yyjson_mut_doc* d, const std::string& s) {
    return yyjson_mut_strncpy(d, s.data(), s.size());
}

void add(yyjson_mut_doc* d, yyjson_mut_val* o, std::string_view k, yyjson_mut_val* v) {
    yyjson_mut_obj_add(o, yyjson_mut_strn(d, k.data(), k.size()), v);
}

yyjson_mut_val* to_json(yyjson_mut_doc* d, const field_failure& v) {
    auto* o = yyjson_mut_obj(d);
    add(d, o, "field", str(d, v.field));
    add(d, o, "code", str(d, v.code));
    add(d, o, "message", str(d, v.message));
    return o;
}

yyjson_mut_val* to_json(yyjson_mut_doc* d, const result& v) {
    auto* o = yyjson_mut_obj(d);
    const auto name = outcome_names[static_cast<std::size_t>(v.outcome)];
    add(d, o, "outcome", yyjson_mut_strn(d, name.data(), name.size()));
    add(d, o, "code", str(d, v.code));
    add(d, o, "message", str(d, v.message));
    auto* fields = yyjson_mut_arr(d);
    for (const auto& f : v.fields)
        yyjson_mut_arr_append(fields, to_json(d, f));
    add(d, o, "fields", fields);
    return o;
}

yyjson_mut_val* to_json(yyjson_mut_doc* d, const change_reason_category& v) {
    auto* o = yyjson_mut_obj(d);
    add(d, o, "version", yyjson_mut_int(d, v.version));
    add(d, o, "tenant_id", owned(d, v.tenant_id.to_string()));
    add(d, o, "code", str(d, v.code));
    add(d, o, "description", str(d, v.description));
    add(d, o, "modified_by", str(d, v.modified_by));
    add(d, o, "performed_by", str(d, v.performed_by));
    add(d, o, "change_reason_code", str(d, v.change_reason_code));
    add(d, o, "change_commentary", str(d, v.change_commentary));
    add(d, o, "recorded_at", owned(d, datetime::to_iso8601_utc(v.recorded_at)));
    return o;
}

std::string text(yyjson_val* v, std::string_view what) {
    if (!yyjson_is_str(v))
        fail(what);
    return {yyjson_get_str(v), yyjson_get_len(v)};
}

template <typename F>
void each_member(yyjson_val* o, std::string_view what, F&& f) {
    if (!yyjson_is_obj(o))
        fail(what);
    yyjson_obj_iter it;
    yyjson_obj_iter_init(o, &it);
    while (yyjson_val* k = yyjson_obj_iter_next(&it))
        f(std::string_view(yyjson_get_str(k), yyjson_get_len(k)), yyjson_obj_iter_get_val(k));
}

void from_json(yyjson_val* o, field_failure& v) {
    unsigned seen = 0;
    each_member(o, "field_failure", [&](std::string_view k, yyjson_val* x) {
        if (k == "field") { v.field = text(x, k); seen |= 1; }
        else if (k == "code") { v.code = text(x, k); seen |= 2; }
        else if (k == "message") { v.message = text(x, k); seen |= 4; }
    });
    if (seen != 7)
        fail("field_failure");
}

void from_json(yyjson_val* o, result& v) {
    unsigned seen = 0;
    each_member(o, "result", [&](std::string_view k, yyjson_val* x) {
        if (k == "outcome") { v.outcome = outcome_from(text(x, k)); seen |= 1; }
        else if (k == "code") { v.code = text(x, k); seen |= 2; }
        else if (k == "message") { v.message = text(x, k); seen |= 4; }
        else if (k == "fields") {
            if (!yyjson_is_arr(x))
                fail(k);
            v.fields.resize(yyjson_arr_size(x));
            std::size_t i = 0;
            yyjson_val* e;
            yyjson_arr_iter it = yyjson_arr_iter_with(x);
            while ((e = yyjson_arr_iter_next(&it)))
                from_json(e, v.fields[i++]);
            seen |= 8;
        }
    });
    if (seen != 15)
        fail("result");
}

void from_json(yyjson_val* o, change_reason_category& v) {
    unsigned seen = 0;
    each_member(o, "change_reason_category", [&](std::string_view k, yyjson_val* x) {
        if (k == "version") {
            if (!yyjson_is_int(x))
                fail(k);
            v.version = static_cast<int>(yyjson_get_sint(x));
            seen |= 1;
        } else if (k == "tenant_id") {
            auto t = ores::utility::uuid::tenant_id::from_string(text(x, k));
            if (!t)
                fail(k);
            v.tenant_id = *t;
            seen |= 2;
        }
        else if (k == "code") { v.code = text(x, k); seen |= 4; }
        else if (k == "description") { v.description = text(x, k); seen |= 8; }
        else if (k == "modified_by") { v.modified_by = text(x, k); seen |= 16; }
        else if (k == "performed_by") { v.performed_by = text(x, k); seen |= 32; }
        else if (k == "change_reason_code") { v.change_reason_code = text(x, k); seen |= 64; }
        else if (k == "change_commentary") { v.change_commentary = text(x, k); seen |= 128; }
        else if (k == "recorded_at") { v.recorded_at = datetime::from_iso8601_utc(text(x, k)); seen |= 256; }
    });
    if (seen != 511)
        fail("change_reason_category");
}

/*
 * MessagePack, packed with the calls reflect-cpp makes, so the bytes match.
 */
void pack(msgpack_packer* p, std::string_view s) {
    msgpack_pack_str(p, s.size());
    msgpack_pack_str_body(p, s.data(), s.size());
}

void pack(msgpack_packer* p, const field_failure& v) {
    msgpack_pack_map(p, 3);
    pack(p, "field"); pack(p, v.field);
    pack(p, "code"); pack(p, v.code);
    pack(p, "message"); pack(p, v.message);
}

void pack(msgpack_packer* p, const result& v) {
    msgpack_pack_map(p, 4);
    pack(p, "outcome"); pack(p, outcome_names[static_cast<std::size_t>(v.outcome)]);
    pack(p, "code"); pack(p, v.code);
    pack(p, "message"); pack(p, v.message);
    pack(p, "fields");
    msgpack_pack_array(p, v.fields.size());
    for (const auto& f : v.fields)
        pack(p, f);
}

void pack(msgpack_packer* p, const change_reason_category& v) {
    msgpack_pack_map(p, 9);
    pack(p, "version"); msgpack_pack_int64(p, v.version);
    pack(p, "tenant_id"); pack(p, v.tenant_id.to_string());
    pack(p, "code"); pack(p, v.code);
    pack(p, "description"); pack(p, v.description);
    pack(p, "modified_by"); pack(p, v.modified_by);
    pack(p, "performed_by"); pack(p, v.performed_by);
    pack(p, "change_reason_code"); pack(p, v.change_reason_code);
    pack(p, "change_commentary"); pack(p, v.change_commentary);
    pack(p, "recorded_at"); pack(p, datetime::to_iso8601_utc(v.recorded_at));
}

std::string text(const msgpack_object& o, std::string_view what) {
    if (o.type != MSGPACK_OBJECT_STR)
        fail(what);
    return {o.via.str.ptr, o.via.str.size};
}

template <typename F>
void each_member(const msgpack_object& o, std::string_view what, F&& f) {
    if (o.type != MSGPACK_OBJECT_MAP)
        fail(what);
    for (std::uint32_t i = 0; i < o.via.map.size; ++i) {
        const auto& kv = o.via.map.ptr[i];
        if (kv.key.type != MSGPACK_OBJECT_STR)
            fail(what);
        f(std::string_view(kv.key.via.str.ptr, kv.key.via.str.size), kv.val);
    }
}

void unpack(const msgpack_object& o, field_failure& v) {
    unsigned seen = 0;
    each_member(o, "field_failure", [&](std::string_view k, const msgpack_object& x) {
        if (k == "field") { v.field = text(x, k); seen |= 1; }
        else if (k == "code") { v.code = text(x, k); seen |= 2; }
        else if (k == "message") { v.message = text(x, k); seen |= 4; }
    });
    if (seen != 7)
        fail("field_failure");
}

void unpack(const msgpack_object& o, result& v) {
    unsigned seen = 0;
    each_member(o, "result", [&](std::string_view k, const msgpack_object& x) {
        if (k == "outcome") { v.outcome = outcome_from(text(x, k)); seen |= 1; }
        else if (k == "code") { v.code = text(x, k); seen |= 2; }
        else if (k == "message") { v.message = text(x, k); seen |= 4; }
        else if (k == "fields") {
            if (x.type != MSGPACK_OBJECT_ARRAY)
                fail(k);
            v.fields.resize(x.via.array.size);
            for (std::uint32_t i = 0; i < x.via.array.size; ++i)
                unpack(x.via.array.ptr[i], v.fields[i]);
            seen |= 8;
        }
    });
    if (seen != 15)
        fail("result");
}

void unpack(const msgpack_object& o, change_reason_category& v) {
    unsigned seen = 0;
    each_member(o, "change_reason_category", [&](std::string_view k, const msgpack_object& x) {
        if (k == "version") {
            if (x.type == MSGPACK_OBJECT_POSITIVE_INTEGER)
                v.version = static_cast<int>(x.via.u64);
            else if (x.type == MSGPACK_OBJECT_NEGATIVE_INTEGER)
                v.version = static_cast<int>(x.via.i64);
            else
                fail(k);
            seen |= 1;
        } else if (k == "tenant_id") {
            auto t = ores::utility::uuid::tenant_id::from_string(text(x, k));
            if (!t)
                fail(k);
            v.tenant_id = *t;
            seen |= 2;
        }
        else if (k == "code") { v.code = text(x, k); seen |= 4; }
        else if (k == "description") { v.description = text(x, k); seen |= 8; }
        else if (k == "modified_by") { v.modified_by = text(x, k); seen |= 16; }
        else if (k == "performed_by") { v.performed_by = text(x, k); seen |= 32; }
        else if (k == "change_reason_code") { v.change_reason_code = text(x, k); seen |= 64; }
        else if (k == "change_commentary") { v.change_commentary = text(x, k); seen |= 128; }
        else if (k == "recorded_at") { v.recorded_at = datetime::from_iso8601_utc(text(x, k)); seen |= 256; }
    });
    if (seen != 511)
        fail("change_reason_category");
}

struct json_doc_deleter {
    void operator()(yyjson_mut_doc* d) const noexcept { yyjson_mut_doc_free(d); }
};
struct json_read_deleter {
    void operator()(yyjson_doc* d) const noexcept { yyjson_doc_free(d); }
};

}

std::string to_json(const response& v) {
    std::unique_ptr<yyjson_mut_doc, json_doc_deleter> d(yyjson_mut_doc_new(nullptr));
    auto* o = yyjson_mut_obj(d.get());
    add(d.get(), o, "result", to_json(d.get(), v.result));
    auto* categories = yyjson_mut_arr(d.get());
    for (const auto& c : v.categories)
        yyjson_mut_arr_append(categories, to_json(d.get(), c));
    add(d.get(), o, "categories", categories);
    add(d.get(), o, "total", yyjson_mut_uint(d.get(), v.total));
    yyjson_mut_doc_set_root(d.get(), o);
    std::size_t n = 0;
    std::unique_ptr<char, decltype(&std::free)> s(
        yyjson_mut_write_opts(d.get(), 0, nullptr, &n, nullptr), &std::free);
    if (!s)
        throw std::runtime_error("Failed to write JSON.");
    return {s.get(), n};
}

response from_json(std::string_view s) {
    std::unique_ptr<yyjson_doc, json_read_deleter> d(
        yyjson_read(s.data(), s.size(), 0));
    if (!d)
        fail("JSON");
    response v;
    unsigned seen = 0;
    each_member(yyjson_doc_get_root(d.get()), "response", [&](std::string_view k, yyjson_val* x) {
        if (k == "result") { from_json(x, v.result); seen |= 1; }
        else if (k == "categories") {
            if (!yyjson_is_arr(x))
                fail(k);
            v.categories.resize(yyjson_arr_size(x));
            std::size_t i = 0;
            yyjson_val* e;
            yyjson_arr_iter it = yyjson_arr_iter_with(x);
            while ((e = yyjson_arr_iter_next(&it)))
                from_json(e, v.categories[i++]);
            seen |= 2;
        } else if (k == "total") {
            if (!yyjson_is_uint(x))
                fail(k);
            v.total = yyjson_get_uint(x);
            seen |= 4;
        }
    });
    if (seen != 7)
        fail("response");
    return v;
}

std::vector<char> to_msgpack(const response& v) {
    msgpack_sbuffer sbuf;
    msgpack_sbuffer_init(&sbuf);
    msgpack_packer p;
    msgpack_packer_init(&p, &sbuf, msgpack_sbuffer_write);
    msgpack_pack_map(&p, 3);
    pack(&p, "result"); pack(&p, v.result);
    pack(&p, "categories");
    msgpack_pack_array(&p, v.categories.size());
    for (const auto& c : v.categories)
        pack(&p, c);
    pack(&p, "total"); msgpack_pack_uint64(&p, v.total);
    std::vector<char> r(sbuf.data, sbuf.data + sbuf.size);
    msgpack_sbuffer_destroy(&sbuf);
    return r;
}

response from_msgpack(std::span<const char> b) {
    msgpack_zone zone;
    msgpack_zone_init(&zone, 2048);
    msgpack_object root;
    const auto rc = msgpack_unpack(b.data(), b.size(), nullptr, &zone, &root);
    if (rc != MSGPACK_UNPACK_SUCCESS && rc != MSGPACK_UNPACK_EXTRA_BYTES) {
        msgpack_zone_destroy(&zone);
        fail("MessagePack");
    }
    response v;
    unsigned seen = 0;
    try {
        each_member(root, "response", [&](std::string_view k, const msgpack_object& x) {
            if (k == "result") { unpack(x, v.result); seen |= 1; }
            else if (k == "categories") {
                if (x.type != MSGPACK_OBJECT_ARRAY)
                    fail(k);
                v.categories.resize(x.via.array.size);
                for (std::uint32_t i = 0; i < x.via.array.size; ++i)
                    unpack(x.via.array.ptr[i], v.categories[i]);
                seen |= 2;
            } else if (k == "total") {
                if (x.type != MSGPACK_OBJECT_POSITIVE_INTEGER)
                    fail(k);
                v.total = x.via.u64;
                seen |= 4;
            }
        });
    } catch (...) {
        msgpack_zone_destroy(&zone);
        throw;
    }
    msgpack_zone_destroy(&zone);
    if (seen != 7)
        fail("response");
    return v;
}

}
