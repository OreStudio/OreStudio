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
#include "ores.http/routes/marketdata/connectivity_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/adapter_protocol.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/feed_scope_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_protocol.hpp"
#include "ores.marketdata.api/messaging/route_protocol.hpp"
#include "ores.marketdata.api/messaging/session_protocol.hpp"
#include "ores.marketdata.api/messaging/supplier_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yyjson.h>

namespace ores::http::routes::marketdata {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace marketdata_domain = ores::marketdata::domain;
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// How long the connectivity catalogue stays fresh: a connection is added rarely.
constexpr std::chrono::seconds connectivity_stale_after{5 * 60};

auto& connectivity_lg() {
    static auto instance = ores::logging::make_logger("ores.http.marketdata.connectivity_routes");
    return instance;
}

/// A comma-separated cell as the list it states.
std::string words_json(const std::string& text) {
    std::vector<std::string> words;
    std::string word;
    for (const char c : text) {
        if (c == ',') {
            const auto trimmed = rr::trimmed(word);
            if (!trimmed.empty())
                words.push_back(json::write(trimmed));
            word.clear();
            continue;
        }
        word += c;
    }
    const auto last = rr::trimmed(word);
    if (!last.empty())
        words.push_back(json::write(last));
    return rr::array_of(words);
}

/**
 * Whether a cell holds a JSON object.
 *
 * The catalogue stores two of its values as documents. A text that does not
 * parse, or that parses as something other than an object, is drawn as an empty
 * document: a screen shows a session with no settings rather than failing the
 * tree for every other record.
 */
bool is_document(std::string_view text) {
    const std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)> doc(
        yyjson_read(text.data(), text.size(), 0), &yyjson_doc_free);
    return doc && yyjson_is_obj(yyjson_doc_get_root(doc.get()));
}

/// The document one catalogue cell holds, or an empty one when it holds none.
std::string document_json(const std::string& text) {
    return is_document(text) ? text : std::string("{}");
}

/// The fields an adapter's configuration schema names, in the order it states them.
std::string schema_json(const std::string& text) {
    if (!is_document(text))
        return "[]";
    const std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)> doc(
        yyjson_read(text.data(), text.size(), 0), &yyjson_doc_free);
    if (!doc)
        return "[]";
    auto* root = yyjson_doc_get_root(doc.get());

    std::vector<std::string> fields;
    auto it = yyjson_obj_iter_with(root);
    while (const auto* name = yyjson_obj_iter_next(&it)) {
        const auto* value = yyjson_obj_iter_get_val(const_cast<yyjson_val*>(name));
        if (value == nullptr || !yyjson_is_str(name))
            continue;

        std::string type("string");
        bool required = false;
        std::string fallback;
        if (yyjson_is_obj(value)) {
            if (const auto* found = yyjson_obj_get(value, "type");
                found != nullptr && yyjson_is_str(found))
                type = yyjson_get_str(found);
            if (const auto* found = yyjson_obj_get(value, "required");
                found != nullptr && yyjson_is_bool(found))
                required = yyjson_get_bool(found);
            if (const auto* found = yyjson_obj_get(value, "default");
                found != nullptr && !yyjson_is_null(found)) {
                if (yyjson_is_str(found)) {
                    fallback = yyjson_get_str(found);
                } else {
                    std::unique_ptr<char, decltype(&std::free)> written(
                        yyjson_val_write(found, 0, nullptr), &std::free);
                    if (written)
                        fallback = written.get();
                }
            }
        }
        fields.push_back("{\"name\":" + json::write(std::string(yyjson_get_str(name))) +
                         ",\"type\":" + json::write(type) + ",\"required\":" +
                         json::write(required) + ",\"default\":" + json::write(fallback) + "}");
    }
    return rr::array_of(fields);
}

/// One adapter's row, its comma lists as lists and its schema as fields.
std::string adapter_json(const marketdata_domain::adapter& adapter) {
    return "{\"code\":" + json::write(adapter.code) + ",\"name\":" + json::write(adapter.name) +
           ",\"module_version\":" + json::write(adapter.module_version) +
           ",\"description\":" + json::write(adapter.description) +
           ",\"process\":" + json::write(adapter.process) +
           ",\"asset_classes\":" + words_json(adapter.asset_classes) +
           ",\"services\":" + words_json(adapter.services) +
           ",\"schema\":" + schema_json(adapter.configuration_schema) + "}";
}

/// One feed session's row, its settings as the document it stores.
std::string session_json(const marketdata_domain::session& session) {
    return "{\"code\":" + json::write(session.code) + ",\"name\":" + json::write(session.name) +
           ",\"supplier_code\":" + json::write(session.supplier_code) +
           ",\"adapter_code\":" + json::write(session.adapter_code) +
           ",\"environment\":" + json::write(session.environment) +
           ",\"endpoint\":" + json::write(session.endpoint) +
           ",\"credential_ref\":" + json::write(session.credential_ref) +
           ",\"settings\":" + document_json(session.settings) +
           ",\"enabled\":" + json::write(session.enabled) +
           ",\"description\":" + json::write(session.description) + "}";
}

/// One publisher's row, its acceptance stated as the one fact the screen reads.
std::string publisher_json(const marketdata_domain::provider& publisher) {
    return "{\"code\":" + json::write(publisher.code) + ",\"name\":" + json::write(publisher.name) +
           ",\"kind\":" + json::write(publisher.kind) +
           ",\"description\":" + json::write(publisher.description) +
           ",\"terms_url\":" + json::write(publisher.terms_url) +
           ",\"terms_credit\":" + json::write(publisher.terms_credit) +
           ",\"terms_accepted\":" + json::write(!publisher.terms_accepted_digest.empty()) +
           ",\"terms_accepted_by\":" + json::write(publisher.terms_accepted_by) +
           ",\"terms_accepted_at\":" + json::write(publisher.terms_accepted_at) + "}";
}

/// One supplier's row.
std::string supplier_json(const marketdata_domain::supplier& supplier) {
    return "{\"code\":" + json::write(supplier.code) + ",\"name\":" + json::write(supplier.name) +
           ",\"kind\":" + json::write(supplier.kind) +
           ",\"description\":" + json::write(supplier.description) +
           ",\"contract_reference\":" + json::write(supplier.contract_reference) +
           ",\"support\":" + json::write(supplier.support) + "}";
}

/// One scope of a feed, as the screen reads it.
std::string scope_json(const marketdata_domain::feed_scope& scope) {
    std::optional<std::string> instrument;
    if (scope.vendor_instrument_id != boost::uuids::nil_uuid())
        instrument = boost::uuids::to_string(scope.vendor_instrument_id);
    return "{\"asset_class\":" + json::write(scope.asset_class) +
           ",\"item\":" + json::write(scope.item) +
           ",\"vendor_instrument_id\":" + json::write(instrument) + "}";
}

/// One route of a feed, as the screen reads it.
std::string route_json(const marketdata_domain::route& route) {
    return "{\"party_id\":" + json::write(boost::uuids::to_string(route.party_id)) +
           ",\"publisher_code\":" + json::write(route.publisher_code) +
           ",\"asset_class\":" + json::write(route.asset_class) + "}";
}

}

http_response connectivity_routes::connectivity_answer(const http_request& req, std::string body) {
    return ores::http::net::view_answer(
        req,
        std::move(body),
        ores::http::net::view_cache_policy::fresh_for(connectivity_stale_after));
}

void connectivity_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto built =
        router->get("/api/marketdata/connectivity")
            .summary("Every catalogue the connectivity screen draws, in one answer")
            .description("A composed view owned by ores.marketdata: the publishers and their "
                         "suppliers, the adapters, the sessions, and the feeds with the scopes "
                         "and the routes each one carries. Seven reads, and the gateway nests "
                         "the children under their feed, splits the adapter's comma lists into "
                         "lists and reads the two columns that hold a document back into one. A "
                         "text that does not parse is drawn as an empty document, so one bad "
                         "record does not fail the tree. The answer carries an ETag and stays "
                         "fresh for 5 minutes.")
            .tags({"marketdata"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_providers_request>([&] {
                    rr::refuse_unknown_params(req, {});

                    auto publishers = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_providers_request{},
                        &marketdata_messaging::list_providers_response::providers);
                    auto suppliers = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_suppliers_request{},
                        &marketdata_messaging::list_suppliers_response::suppliers);
                    auto adapters = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_adapters_request{},
                        &marketdata_messaging::list_adapters_response::adapters);
                    auto sessions = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_sessions_request{},
                        &marketdata_messaging::list_sessions_response::sessions);
                    auto feeds =
                        forwarding::forward_all(req,
                                                session,
                                                marketdata_messaging::list_feeds_request{},
                                                &marketdata_messaging::list_feeds_response::feeds);
                    auto scopes = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_feed_scopes_request{},
                        &marketdata_messaging::list_feed_scopes_response::scopes);
                    auto routes = forwarding::forward_all(
                        req,
                        session,
                        marketdata_messaging::list_routes_request{},
                        &marketdata_messaging::list_routes_response::routes);

                    std::ranges::sort(publishers, {}, &marketdata_domain::provider::name);
                    std::ranges::sort(suppliers, {}, &marketdata_domain::supplier::name);
                    std::ranges::sort(adapters, {}, &marketdata_domain::adapter::name);
                    std::ranges::sort(sessions, {}, &marketdata_domain::session::code);
                    std::ranges::sort(feeds, {}, &marketdata_domain::feed::name);

                    std::vector<std::string> publisher_rows;
                    for (const auto& publisher : publishers)
                        publisher_rows.push_back(publisher_json(publisher));
                    std::vector<std::string> supplier_rows;
                    for (const auto& supplier : suppliers)
                        supplier_rows.push_back(supplier_json(supplier));
                    std::vector<std::string> adapter_rows;
                    for (const auto& adapter : adapters)
                        adapter_rows.push_back(adapter_json(adapter));
                    std::vector<std::string> session_rows;
                    for (const auto& one : sessions)
                        session_rows.push_back(session_json(one));

                    std::vector<std::string> feed_rows;
                    feed_rows.reserve(feeds.size());
                    for (const auto& feed : feeds) {
                        const auto key = boost::uuids::to_string(feed.id);
                        std::vector<const marketdata_domain::feed_scope*> mine;
                        for (const auto& scope : scopes)
                            if (boost::uuids::to_string(scope.feed_id) == key)
                                mine.push_back(&scope);
                        std::ranges::sort(mine, {}, &marketdata_domain::feed_scope::item);
                        std::vector<std::string> feed_scopes;
                        feed_scopes.reserve(mine.size());
                        for (const auto* scope : mine)
                            feed_scopes.push_back(scope_json(*scope));
                        std::vector<std::string> feed_routes;
                        for (const auto& route : routes)
                            if (boost::uuids::to_string(route.feed_id) == key)
                                feed_routes.push_back(route_json(route));

                        feed_rows.push_back(
                            "{\"id\":" + json::write(key) + ",\"name\":" + json::write(feed.name) +
                            ",\"publisher_code\":" + json::write(feed.publisher_code) +
                            ",\"session_code\":" + json::write(feed.session_code) +
                            ",\"service\":" + json::write(feed.service) +
                            ",\"provider_key\":" + json::write(feed.provider_key) +
                            ",\"is_default\":" + json::write(feed.is_default) +
                            ",\"description\":" + json::write(feed.description) +
                            ",\"scopes\":" + rr::array_of(feed_scopes) +
                            ",\"routes\":" + rr::array_of(feed_routes) + "}");
                    }

                    ORES_LOG_SEV(connectivity_lg(), ores::logging::debug)
                        << "The connectivity catalogue holds " << publisher_rows.size()
                        << " publishers, " << feed_rows.size() << " feeds and " << scopes.size()
                        << " scopes";
                    return connectivity_answer(req,
                                               "{\"publishers\":" + rr::array_of(publisher_rows) +
                                                   ",\"suppliers\":" + rr::array_of(supplier_rows) +
                                                   ",\"adapters\":" + rr::array_of(adapter_rows) +
                                                   ",\"sessions\":" + rr::array_of(session_rows) +
                                                   ",\"feeds\":" + rr::array_of(feed_rows) + "}");
                });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
