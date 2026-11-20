/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.http/routes/analytics/sensitivity_template_routes.hpp"
#include "ores.analytics.api/messaging/sensitivity_asset_class_protocol.hpp"
#include "ores.analytics.api/messaging/sensitivity_method_protocol.hpp"
#include "ores.analytics.api/messaging/sensitivity_template_protocol.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http.nats/history_read.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <charconv>
#include <chrono>
#include <cstdint>
#include <exception>
#include <functional>
#include <limits>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace ores::http::routes::analytics {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace messaging = ores::analytics::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The entity type one key's history is read by.
constexpr std::string_view entity_type = "ores.analytics.sensitivity_template";

/// The lookups hold fewer rows than this, so one page reads them all.
constexpr std::uint32_t choice_page = 1000;

/// How long a page of keys stays fresh: the screen reloads after a write.
constexpr std::chrono::seconds page_stale_after{5};

/// The sentence a body is refused with when it states another shape.
constexpr std::string_view body_refused = "A sensitivity template states its code, its method, "
                                          "the asset class and why it is written.";

/// The sentence a key is refused with when the address names none.
constexpr std::string_view code_refused = "A sensitivity template is named by its code.";

/// One method, as the screen chooses it.
struct method_choice final {
    std::string code;
    std::string name;
    bool needs_asset_class{};
    int display_order{};
};

/// One asset class ORE presents a template prefix for.
struct asset_class_choice final {
    std::string code;
    std::string prefix;
    std::string name;
    int display_order{};
};

/// The two lookups, in the order the screen offers them.
struct choices final {
    std::vector<asset_class_choice> asset_classes;
    std::vector<method_choice> methods;
};

/// The body a create states.
struct create_body final {
    std::string code;
    std::string methodCode;
    std::optional<std::string> assetClassCode;
    std::string description;
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<create_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"code", &create_body::code},
                                            field{"methodCode", &create_body::methodCode},
                                            field{"assetClassCode", &create_body::assetClassCode},
                                            field{"description", &create_body::description},
                                            field{"reasonCode", &create_body::reasonCode},
                                            field{"commentary", &create_body::commentary}};
}

/// The body a correction states: the key is fixed once it exists, so no code.
struct update_body final {
    std::string methodCode;
    std::optional<std::string> assetClassCode;
    std::string description;
    std::uint32_t version{};
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<update_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"methodCode", &update_body::methodCode},
                                            field{"assetClassCode", &update_body::assetClassCode},
                                            field{"description", &update_body::description},
                                            field{"version", &update_body::version},
                                            field{"reasonCode", &update_body::reasonCode},
                                            field{"commentary", &update_body::commentary}};
}

/// The body a removal states.
struct remove_body final {
    std::uint32_t version{};
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<remove_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"version", &remove_body::version},
                                            field{"reasonCode", &remove_body::reasonCode},
                                            field{"commentary", &remove_body::commentary}};
}

/// A whole number the address states, or nothing when it is not one.
std::optional<std::uint32_t> count_of(const http_request& req,
                                      const std::string& name,
                                      std::uint32_t fallback,
                                      std::uint32_t least,
                                      std::uint32_t most) {
    const auto text = req.get_query_param(name);
    if (text.empty())
        return fallback;
    std::uint32_t value = 0;
    const auto [end, ec] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (ec != std::errc{} || end != text.data() + text.size() || value < least || value > most)
        return std::nullopt;
    return value;
}

/// The pair a write states, refused when it is not one the tables allow.
///
/// The rule is the methods table's: a method that needs no class states none.
/// The store refuses it too, and this says which of the two is wrong before the
/// write reaches it.
void pair_of(const choices& known,
             const std::string& method_code,
             std::optional<std::string> asset_class_code,
             std::string& out_method,
             std::string& out_asset_class) {
    const auto method = std::ranges::find(known.methods, method_code, &method_choice::code);
    if (method == known.methods.end()) {
        std::string names;
        for (const auto& candidate : known.methods) {
            if (!names.empty())
                names += ", ";
            names += candidate.name;
        }
        rr::invalid((method_code.empty() ? std::string("A template") : method_code) +
                    " is not a method a template may state. The methods are " + names + ".");
    }
    if (method->needs_asset_class && !asset_class_code)
        rr::invalid("A " + method->name +
                    " template states the asset class of the product it "
                    "belongs to.");
    if (!method->needs_asset_class && asset_class_code)
        rr::invalid("A " + method->name + " template states no asset class.");
    out_method = method->code;
    out_asset_class = asset_class_code.value_or("");
}

/// The methods and the asset classes a template may name, read from their tables.
choices read_choices(const http_request& req, nats_client& session) {
    messaging::list_sensitivity_methods_request methods_msg;
    methods_msg.limit = choice_page;
    const auto methods = forwarding::forward(req, session, methods_msg);
    rr::require_ok(methods.result);

    messaging::list_sensitivity_asset_classes_request classes_msg;
    classes_msg.limit = choice_page;
    const auto classes = forwarding::forward(req, session, classes_msg);
    rr::require_ok(classes.result);

    choices known;
    known.methods.reserve(methods.methods.size());
    for (const auto& method : methods.methods)
        known.methods.push_back(
            {method.code, method.name, method.needs_asset_class, method.display_order});
    known.asset_classes.reserve(classes.asset_classes.size());
    for (const auto& asset_class : classes.asset_classes)
        known.asset_classes.push_back({asset_class.asset_class_code,
                                       asset_class.template_prefix,
                                       asset_class.name,
                                       asset_class.display_order});

    // The tables state the order the screen offers them in, so the part does not.
    std::ranges::sort(known.methods, std::ranges::less{}, &method_choice::display_order);
    std::ranges::sort(known.asset_classes, std::ranges::less{}, &asset_class_choice::display_order);
    return known;
}

/// One key as the screen reads it: the two codes it states, and the words and
/// the prefix the lookups hold for them.
std::string template_json(const ores::analytics::domain::sensitivity_template& stored,
                          const choices& known) {
    const auto method = std::ranges::find(known.methods, stored.method_code, &method_choice::code);
    const auto asset_class =
        std::ranges::find(known.asset_classes, stored.asset_class_code, &asset_class_choice::code);
    return "{\"code\":" + json::write(stored.code) +
           ",\"methodCode\":" + json::write(stored.method_code) + ",\"methodName\":" +
           json::write(method == known.methods.end() ? stored.method_code : method->name) +
           ",\"assetClassCode\":" + json::write(stored.asset_class_code) + ",\"assetClassName\":" +
           json::write(asset_class == known.asset_classes.end() ? stored.asset_class_code :
                                                                  asset_class->name) +
           ",\"templatePrefix\":" +
           json::write(asset_class == known.asset_classes.end() ? std::string() :
                                                                  asset_class->prefix) +
           ",\"description\":" + json::write(stored.description) +
           ",\"version\":" + std::to_string(stored.version) +
           ",\"modifiedBy\":" + json::write(stored.modified_by) +
           ",\"performedBy\":" + json::write(stored.performed_by) +
           ",\"reasonCode\":" + json::write(stored.change_reason_code) +
           ",\"commentary\":" + json::write(stored.change_commentary) +
           ",\"recordedAt\":" + json::write(stored.recorded_at) + "}";
}

/// One key of the tenant, or a not-found that names it.
std::string read_one(const http_request& req,
                     nats_client& session,
                     const std::string& code,
                     const choices& known) {
    messaging::get_sensitivity_template_request msg;
    msg.key = messaging::sensitivity_template_key{code};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    if (!reply.sensitivity_template)
        rr::refuse(
            http_status::not_found, "not-found", "There is no sensitivity template " + code + ".");
    return template_json(*reply.sensitivity_template, known);
}

/// The code a screen states: 1 to 100 characters once trimmed.
///
/// The address and the body state the same key, so both are trimmed and both are
/// measured the way the rest of the part measures text, in characters and not in
/// bytes, so a code with an accent in it is counted once.
std::string code_from(const std::string& raw) {
    const auto code = rr::trimmed(raw);
    if (code.empty() || rr::characters_of(code) > 100)
        rr::invalid(std::string(code_refused));
    return code;
}

/// The code the address states.
std::string code_of(const http_request& req) {
    return code_from(req.get_path_param("code"));
}

/// The address's query as the page reads it.
struct list_query final {
    std::uint32_t offset{};
    std::uint32_t limit{};
    std::string search;
    std::string sort;
    bool descending{};
};

std::optional<list_query> read_page_query(const http_request& req) {
    const auto offset = count_of(req, "offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
    const auto limit = count_of(req, "limit", 15, 1, 1000);
    const auto search = rr::trimmed(req.get_query_param("search"));
    const auto sort = req.get_query_param("sort");
    const auto descending = req.get_query_param("descending");
    if (!offset || !limit || rr::characters_of(search) > 256 || sort.size() > 100 ||
        (descending != "" && descending != "true" && descending != "false"))
        return std::nullopt;
    return list_query{*offset, *limit, search, sort, descending == "true"};
}

}

void sensitivity_template_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.tags({"analytics"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/analytics/sensitivity-templates"), [&session](const http_request& req) {
        return forwarding::guarded<messaging::list_sensitivity_templates_request>([&] {
            rr::refuse_unknown_params(req, {"offset", "limit", "search", "sort", "descending"});
            const auto page = read_page_query(req);
            if (!page)
                return rr::refused(http_status::bad_request,
                                   "invalid-request",
                                   "A page of keys names an offset, a limit of 1 to 1000, a "
                                   "search, and an order by code.");
            const auto known = read_choices(req, session);
            messaging::list_sensitivity_templates_request msg;
            msg.offset = page->offset;
            msg.limit = page->limit;
            msg.order.field = page->sort;
            msg.order.descending = page->descending;
            msg.filter.emplace();
            msg.filter->search =
                page->search.empty() ? std::nullopt : std::optional<std::string>(page->search);
            const auto reply = forwarding::forward(req, session, msg);
            rr::require_ok(reply.result);

            std::vector<std::string> rows;
            rows.reserve(reply.templates.size());
            for (const auto& stored : reply.templates)
                rows.push_back(template_json(stored, known));
            return ores::http::net::view_answer(
                req,
                "{\"rows\":" + rr::array_of(rows) + ",\"total\":" + std::to_string(reply.total) +
                    "}",
                ores::http::net::view_cache_policy::fresh_for(page_stale_after));
        });
    });

    add(router->get("/api/analytics/sensitivity-templates/choices"),
        [&session](const http_request& req) {
            return forwarding::guarded<messaging::list_sensitivity_methods_request>([&] {
                rr::refuse_unknown_params(req, {});
                const auto known = read_choices(req, session);
                std::vector<std::string> classes;
                classes.reserve(known.asset_classes.size());
                for (const auto& asset_class : known.asset_classes)
                    classes.push_back(
                        rr::object_of({"\"code\":" + json::write(asset_class.code),
                                       "\"prefix\":" + json::write(asset_class.prefix),
                                       "\"name\":" + json::write(asset_class.name)}));
                std::vector<std::string> methods;
                methods.reserve(known.methods.size());
                for (const auto& method : known.methods)
                    methods.push_back(rr::object_of(
                        {"\"code\":" + json::write(method.code),
                         "\"name\":" + json::write(method.name),
                         "\"needsAssetClass\":" +
                             std::string(method.needs_asset_class ? "true" : "false")}));
                return http_response::json("{\"assetClasses\":" + rr::array_of(classes) +
                                           ",\"methods\":" + rr::array_of(methods) + "}");
            });
        });

    add(router->get("/api/analytics/sensitivity-templates/{code}"),
        [&session](const http_request& req) {
            return forwarding::guarded<messaging::get_sensitivity_template_request>([&] {
                rr::refuse_unknown_params(req, {});
                const auto code = code_of(req);
                const auto known = read_choices(req, session);
                return http_response::json("{\"template\":" + read_one(req, session, code, known) +
                                           "}");
            });
        });

    add(router->get("/api/analytics/sensitivity-templates/{code}/history"),
        [&session](const http_request& req) {
            // The history step names its own subject, so there is no request type
            // for guard to log a failure against; this route states its failure
            // handling instead.
            try {
                rr::refuse_unknown_params(req, {});
                const auto code = code_of(req);
                const auto answer = ores::http::net::history::read(req, session, entity_type, code);
                if (!answer.answered)
                    rr::refuse(http_status::bad_gateway,
                               "upstream-unavailable",
                               answer.message.empty() ? std::string(rr::unanswered) :
                                                        answer.message);
                std::vector<std::string> versions;
                versions.reserve(answer.versions.size());
                for (const auto& version : answer.versions) {
                    std::vector<std::string> fields;
                    fields.reserve(version.fields.size());
                    for (const auto& field : version.fields)
                        fields.push_back(rr::object_of({"\"name\":" + json::write(field.name),
                                                        "\"value\":" + json::write(field.value)}));
                    std::vector<std::string> changes;
                    changes.reserve(version.changes.size());
                    for (const auto& change : version.changes)
                        changes.push_back(
                            rr::object_of({"\"field\":" + json::write(change.field),
                                           "\"before\":" + json::write(change.before),
                                           "\"after\":" + json::write(change.after)}));
                    versions.push_back(
                        rr::object_of({"\"version\":" + std::to_string(version.version),
                                       "\"modifiedBy\":" + json::write(version.modified_by),
                                       "\"recordedAt\":" + json::write(version.recorded_at),
                                       "\"fields\":" + rr::array_of(fields),
                                       "\"changes\":" + rr::array_of(changes)}));
                }
                return http_response::json("{\"versions\":" + rr::array_of(versions) + "}");
            } catch (const rr::refusal& refusal) {
                return rr::answer(refusal);
            } catch (const std::exception& e) {
                return forwarding::unanswered("the sensitivity template's history", e);
            }
        });

    add(router->post("/api/analytics/sensitivity-templates"), [&session](const http_request& req) {
        return forwarding::guarded<messaging::put_sensitivity_template_request>([&] {
            const auto body = rr::body_of<create_body>(
                req,
                {"code", "methodCode", "assetClassCode", "description", "reasonCode", "commentary"},
                std::string(body_refused));
            const auto code = code_from(body.code);
            const auto known = read_choices(req, session);
            std::string method_code;
            std::string asset_class_code;
            pair_of(known,
                    rr::trimmed(body.methodCode),
                    body.assetClassCode,
                    method_code,
                    asset_class_code);

            messaging::put_sensitivity_template_request msg;
            msg.change.write = {code, method_code, asset_class_code, body.description};
            msg.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
            msg.intent = {rr::trimmed(body.reasonCode), body.commentary};
            const auto reply = forwarding::forward(req, session, msg);
            rr::require_ok(reply.result);
            const auto written = reply.sensitivity_template ?
                                     template_json(*reply.sensitivity_template, known) :
                                     read_one(req, session, code, known);
            return http_response::json("{\"template\":" + written + "}", http_status::created);
        });
    });

    add(router->put("/api/analytics/sensitivity-templates/{code}"),
        [&session](const http_request& req) {
            return forwarding::guarded<messaging::put_sensitivity_template_request>([&] {
                const auto code = code_of(req);
                const auto body = rr::body_of<update_body>(req,
                                                           {"methodCode",
                                                            "assetClassCode",
                                                            "description",
                                                            "version",
                                                            "reasonCode",
                                                            "commentary"},
                                                           std::string(body_refused));
                const auto known = read_choices(req, session);
                std::string method_code;
                std::string asset_class_code;
                pair_of(known,
                        rr::trimmed(body.methodCode),
                        body.assetClassCode,
                        method_code,
                        asset_class_code);

                messaging::put_sensitivity_template_request msg;
                msg.change.write = {code, method_code, asset_class_code, body.description};
                msg.change.precondition.kind =
                    ores::utility::domain::precondition_kind::must_match_version;
                msg.change.precondition.version = body.version;
                msg.intent = {rr::trimmed(body.reasonCode), body.commentary};
                const auto reply = forwarding::forward(req, session, msg);
                rr::require_ok(reply.result);
                const auto written = reply.sensitivity_template ?
                                         template_json(*reply.sensitivity_template, known) :
                                         read_one(req, session, code, known);
                return http_response::json("{\"template\":" + written + "}");
            });
        });

    add(router->delete_("/api/analytics/sensitivity-templates/{code}"),
        [&session](const http_request& req) {
            return forwarding::guarded<messaging::delete_sensitivity_template_request>([&] {
                const auto code = code_of(req);
                const auto body = rr::body_of<remove_body>(
                    req, {"version", "reasonCode", "commentary"}, std::string(body_refused));
                const auto known = read_choices(req, session);
                const auto removed = read_one(req, session, code, known);

                messaging::delete_sensitivity_template_request msg;
                msg.removal.key = messaging::sensitivity_template_key{code};
                msg.removal.precondition.kind =
                    ores::utility::domain::precondition_kind::must_match_version;
                msg.removal.precondition.version = body.version;
                msg.intent = {rr::trimmed(body.reasonCode), body.commentary};
                const auto reply = forwarding::forward(req, session, msg);
                rr::require_ok(reply.result);
                return http_response::json("{\"removed\":" + removed + "}");
            });
        });
}

}
