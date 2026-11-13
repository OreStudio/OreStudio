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
#include "ores.http/routes/trading/structure_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.trading.api/domain/product_type.hpp"
#include "ores.trading.api/domain/product_type_asset_class.hpp"
#include "ores.trading.api/domain/structure_identifier.hpp"
#include "ores.trading.api/domain/structure_kind.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/domain/trade_summary.hpp"
#include "ores.trading.api/messaging/product_type_asset_class_protocol.hpp"
#include "ores.trading.api/messaging/product_type_protocol.hpp"
#include "ores.trading.api/messaging/structure_identifier_protocol.hpp"
#include "ores.trading.api/messaging/structure_kind_protocol.hpp"
#include "ores.trading.api/messaging/structure_member_protocol.hpp"
#include "ores.trading.api/messaging/structure_protocol.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <charconv>
#include <chrono>
#include <cstddef>
#include <cstdlib>
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yyjson.h>

namespace ores::http::routes::trading {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace trading_domain = ores::trading::domain;
namespace trading_messaging = ores::trading::messaging;
namespace refdata_messaging = ores::refdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace structures {

/// How many trades the add-a-leg picker offers, newest first.
constexpr std::uint32_t candidate_page = 200;

/// The one role a leg may take when the structure names no template of its own.
constexpr std::string_view free_role = "leg";

/// The parent a root states, which the store holds as the nil uuid.
const boost::uuids::uuid no_parent = {};

/// Why a leg or a grouping was written.
constexpr std::string_view new_record = "system.new_record";

/// Why a leg was taken out of a structure.
constexpr std::string_view amend_record = "system.amend_record";

/// The note a grouping states, so the next reader knows where it came from.
constexpr std::string_view grouped_note = "Grouped from the grouping screen.";

std::string text_of(const boost::uuids::uuid& id) {
    return boost::uuids::to_string(id);
}

std::string text_of(const std::optional<boost::uuids::uuid>& id) {
    return id ? boost::uuids::to_string(*id) : std::string();
}

/// The bodies the structure screens send, beside the wire_fields each is used with.
/// The grouping a person states: a kind, and the trades to put in it.
struct group_body final {
    std::string kind;
    std::vector<std::string> tradeIds;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<group_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"kind", &group_body::kind},
                                              field{"tradeIds", &group_body::tradeIds}};
}

/// A leg a person puts into a structure, and why.
struct leg_body final {
    std::string tradeId;
    std::string role;
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<leg_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"tradeId", &leg_body::tradeId},
                                              field{"role", &leg_body::role},
                                              field{"commentary", &leg_body::commentary}};
}

/// Why a person takes a leg out.
struct unlink_body final {
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<unlink_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"commentary", &unlink_body::commentary}};
}

/// The structures a tenant may see.
std::vector<trading_domain::structure> all_structures(const http_request& req,
                                                      nats_client& session) {
    trading_messaging::list_structures_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &trading_messaging::list_structures_response::structures);
}

/// The members a tenant may see, which is every leg of every deal.
std::vector<trading_domain::structure_member> all_members(const http_request& req,
                                                          nats_client& session) {
    trading_messaging::list_structure_members_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &trading_messaging::list_structure_members_response::members);
}

/// The reference each structure carries, keyed by the structure's id.
std::map<std::string, std::string> structure_references(
    const http_request& req, nats_client& session, const std::vector<boost::uuids::uuid>& ids) {
    std::map<std::string, std::string> references;
    if (ids.empty())
        return references;
    trading_messaging::list_structure_identifiers_request msg;
    msg.filter.emplace();
    msg.filter->structure_id_one_of = ids;
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_structure_identifiers_response::identifiers);
    for (const auto& row : rows) {
        if (row.id_value.empty())
            continue;
        references.emplace(text_of(row.structure_id), row.id_value);
    }
    return references;
}

/// The reference each trade carries, keyed by the trade's id.
std::map<std::string, std::string> trade_references(const http_request& req,
                                                    nats_client& session,
                                                    const std::vector<boost::uuids::uuid>& ids) {
    std::map<std::string, std::string> references;
    if (ids.empty())
        return references;
    trading_messaging::list_trade_identifiers_request msg;
    msg.filter.emplace();
    msg.filter->trade_id_one_of = ids;
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trade_identifiers_response::identifiers);
    for (const auto& row : rows) {
        if (row.id_value.empty())
            continue;
        references.emplace(text_of(row.trade_id), row.id_value);
    }
    return references;
}

/// The trades a set of ids names, with the facts a tree row and a leg states.
std::map<std::string, trading_domain::trade> trades_by_ids(
    const http_request& req, nats_client& session, const std::vector<boost::uuids::uuid>& ids) {
    std::map<std::string, trading_domain::trade> trades;
    if (ids.empty())
        return trades;
    trading_messaging::list_trades_request msg;
    msg.filter.emplace();
    msg.filter->id_one_of = ids;
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trades_response::trades);
    for (const auto& row : rows)
        trades.emplace(text_of(row.id), row);
    return trades;
}

/// The name each counterparty is known by, keyed by its id.
std::map<std::string, std::string> counterparty_names(const http_request& req,
                                                      nats_client& session,
                                                      const std::vector<boost::uuids::uuid>& ids) {
    std::map<std::string, std::string> names;
    if (ids.empty())
        return names;
    refdata_messaging::list_counterparties_request msg;
    msg.filter.emplace();
    msg.filter->id_one_of = ids;
    const auto rows = forwarding::forward_all(
        req, session, msg, &refdata_messaging::list_counterparties_response::counterparties);
    for (const auto& row : rows)
        names.emplace(text_of(row.id), row.full_name);
    return names;
}

/// The name each trade type is known by, and the asset class it belongs to.
struct catalogue final {
    std::map<std::string, std::string> names;
    std::map<std::string, std::string> classes;
};

catalogue catalogue_of(const http_request& req, nats_client& session) {
    trading_messaging::list_product_types_request types_msg;
    types_msg.filter.emplace();
    const auto types = forwarding::forward_all(
        req, session, types_msg, &trading_messaging::list_product_types_response::types);
    trading_messaging::list_product_type_asset_classes_request markets_msg;
    const auto markets = forwarding::forward_all(
        req,
        session,
        markets_msg,
        &trading_messaging::list_product_type_asset_classes_response::product_type_asset_classes);
    /* The market the catalogue groups a type under is the one it marks home. */
    std::map<std::string, std::string> home_of;
    for (const auto& market : markets)
        if (market.is_home)
            home_of[market.product_type_code] = market.asset_class_code;
    catalogue out;
    for (const auto& type : types) {
        out.names[type.code] = type.name.empty() ? type.description : type.name;
        const auto home = home_of.find(type.code);
        out.classes[type.code] = home == home_of.end() ? std::string() : home->second;
    }
    return out;
}

/// The state each trade is in, keyed by the trade's id.
std::map<std::string, std::string> states_of(const http_request& req,
                                             nats_client& session,
                                             const std::vector<boost::uuids::uuid>& ids) {
    std::map<std::string, std::string> states;
    if (ids.empty())
        return states;
    trading_messaging::list_trade_summaries_request msg;
    msg.filter.emplace();
    msg.filter->id_one_of = ids;
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trade_summaries_response::trade_summaries);
    for (const auto& row : rows)
        states[text_of(row.id)] = row.status.value_or(std::string());
    return states;
}

/// The trades the picker offers: a page of one-row summaries, newest first.
std::vector<trading_domain::trade_summary> recent_trades(const http_request& req,
                                                         nats_client& session) {
    trading_messaging::list_trade_summaries_request msg;
    msg.filter.emplace();
    msg.filter->row_kind = "trade";
    msg.order.field = "trade_date";
    msg.order.descending = true;
    msg.limit = candidate_page;
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trade_summaries_response::trade_summaries);
    return rows;
}

/// The summaries of the whole trade list, which the grouping checks its trades against.
std::vector<trading_domain::trade_summary> all_summaries(const http_request& req,
                                                         nats_client& session) {
    trading_messaging::list_trade_summaries_request msg;
    msg.filter.emplace();
    return forwarding::forward_all(
        req, session, msg, &trading_messaging::list_trade_summaries_response::trade_summaries);
}

/// The key a structure is addressed by: trimmed, and 1 to 64 characters.
std::string key_of(const http_request& req) {
    const std::string sentence =
        "A structure is addressed by its own id or by its reference, of 1 to 64 characters.";
    const auto key = rr::trimmed(req.get_path_param("key"));
    if (key.empty() || rr::characters_of(key) > 64)
        rr::invalid(sentence);
    return key;
}

/// The structure a key names, or a refusal.
///
/// A reference is resolved against the identifiers the tenant holds, because the
/// store answers identifiers by structure and not by value.
trading_domain::structure
resolve_structure(const http_request& req, nats_client& session, const std::string& key) {
    const std::string missing = "No structure has that reference.";
    auto structure_id = key;
    if (!rr::is_uuid(key)) {
        trading_messaging::list_structure_identifiers_request msg;
        msg.filter.emplace();
        const auto rows = forwarding::forward_all(
            req,
            session,
            msg,
            &trading_messaging::list_structure_identifiers_response::identifiers);
        std::string lowered = key;
        std::transform(lowered.begin(), lowered.end(), lowered.begin(), [](unsigned char c) {
            return static_cast<char>(std::tolower(c));
        });
        structure_id.clear();
        for (const auto& row : rows) {
            std::string value = row.id_value;
            std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) {
                return static_cast<char>(std::tolower(c));
            });
            if (value == lowered) {
                structure_id = text_of(row.structure_id);
                break;
            }
        }
        if (structure_id.empty())
            rr::refuse(http_status::not_found, "not-found", missing);
    }
    trading_messaging::list_structures_request msg;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{boost::uuids::string_generator()(structure_id)};
    const auto rows = forwarding::forward_all(
        req, session, msg, &trading_messaging::list_structures_response::structures);
    if (rows.empty())
        rr::refuse(http_status::not_found, "not-found", missing);
    return rows.front();
}

/// A leg a body states, checked as the BFF checked it.
trading_messaging::put_structure_member_request member_of(const http_request& req) {
    const std::string sentence =
        "A leg states a trade, a role of 1 to 64 characters and a reason of 1 to 500 characters.";
    rr::refuse_unknown_members(req.body, {"tradeId", "role", "commentary"}, sentence);
    const auto body = rr::read_body<leg_body>(req.body);
    if (!body)
        rr::invalid(sentence);
    const auto role = rr::trimmed(body->role);
    const auto commentary = rr::trimmed(body->commentary);
    if (!rr::is_uuid(body->tradeId) || role.empty() || rr::characters_of(role) > 64 ||
        commentary.empty() || rr::characters_of(commentary) > 500)
        rr::invalid(sentence);
    if (role != free_role)
        rr::invalid("The structure does not offer that role.");
    trading_messaging::put_structure_member_request msg;
    msg.change.write.trade_id = boost::uuids::string_generator()(body->tradeId);
    msg.change.write.role = role;
    msg.change.precondition.kind = ores::utility::domain::precondition_kind::must_not_exist;
    msg.intent = {std::string(new_record), commentary};
    return msg;
}

/// Why a leg is taken out, checked as the BFF checked it.
std::string unlink_reason_of(const http_request& req) {
    const std::string sentence =
        "A removal states why the leg is taken out, in 1 to 500 characters.";
    rr::refuse_unknown_members(req.body, {"commentary"}, sentence);
    const auto body = rr::read_body<unlink_body>(req.body);
    if (!body)
        rr::invalid(sentence);
    const auto commentary = rr::trimmed(body->commentary);
    if (commentary.empty() || rr::characters_of(commentary) > 500)
        rr::invalid(sentence);
    return commentary;
}

/// The type's name as a person reads it: the catalogue's, or the code humanised.
std::string name_of(const catalogue& named, const std::string& type) {
    const auto found = named.names.find(type);
    const auto name = found == named.names.end() ? std::string() : found->second;
    if (!name.empty() && name != type)
        return name;
    return structure_routes::humanised(type);
}

/// The asset class the type belongs to, as the desk says it.
std::string class_of(const catalogue& named, const std::string& type) {
    const auto found = named.classes.find(type);
    return structure_routes::asset_class_named(found == named.classes.end() ? std::string() :
                                                                              found->second);
}

/// One structure row of the list, as the screen draws it.
struct list_row final {
    std::string id;
    std::string reference;
    std::string kind;
    std::string template_code;
    std::string counterparty_id;
    std::string counterparty_name;
    std::size_t legs{};
    std::size_t inner_structures{};
    std::string confirms;
    std::string prices;
    std::string parent_id;
    int depth{};
};

/// The given ids, without repeats, in the order first named.
std::vector<boost::uuids::uuid> distinct(const std::vector<boost::uuids::uuid>& ids) {
    std::vector<boost::uuids::uuid> out;
    for (const auto& id : ids)
        if (std::ranges::find(out, id) == out.end())
            out.push_back(id);
    return out;
}

/// The trades no structure holds yet, newest first, with the reference each one
/// carries.
std::pair<std::vector<trading_domain::trade_summary>, std::map<std::string, std::string>>
unheld_trades(const http_request& req, nats_client& session) {
    const auto members = all_members(req, session);
    std::vector<boost::uuids::uuid> taken;
    for (const auto& member : members)
        taken.push_back(member.trade_id);
    std::vector<trading_domain::trade_summary> free;
    std::vector<boost::uuids::uuid> free_ids;
    for (const auto& row : recent_trades(req, session)) {
        if (row.row_kind != "trade" || std::ranges::find(taken, row.id) != taken.end())
            continue;
        free.push_back(row);
        free_ids.push_back(row.id);
    }
    return {std::move(free), trade_references(req, session, distinct(free_ids))};
}

}

}

std::string structure_routes::humanised(const std::string& code) {
    std::string spaced;
    for (std::size_t i = 0; i < code.size(); ++i) {
        const char current = code[i];
        if (i > 0 && std::isupper(static_cast<unsigned char>(current))) {
            const char previous = code[i - 1];
            const bool after_word = std::islower(static_cast<unsigned char>(previous)) != 0 ||
                                    std::isdigit(static_cast<unsigned char>(previous)) != 0;
            const bool ends_acronym = std::isupper(static_cast<unsigned char>(previous)) != 0 &&
                                      i + 1 < code.size() &&
                                      std::islower(static_cast<unsigned char>(code[i + 1])) != 0;
            if (after_word || ends_acronym)
                spaced += ' ';
        }
        spaced += current;
    }
    static constexpr std::pair<std::string_view, std::string_view> prefixes[] = {{"Fx ", "FX "},
                                                                                 {"Cds ", "CDS "},
                                                                                 {"Cms ", "CMS "},
                                                                                 {"Irs ", "IRS "},
                                                                                 {"Ois ", "OIS "},
                                                                                 {"Var ", "VaR "}};
    for (const auto& [from, to] : prefixes) {
        if (spaced.starts_with(from)) {
            spaced.replace(0, from.size(), to);
            break;
        }
    }
    return spaced;
}

std::string structure_routes::asset_class_named(const std::string& code) {
    if (code == "interest_rates")
        return "Rates";
    if (code == "fx")
        return "FX";
    if (code == "bond")
        return "Bond";
    return code.empty() ? std::string() : humanised(code);
}

structure_routes::verdict structure_routes::verdicts_of(const std::string& kind) {
    const bool as_one = kind != "Package";
    return {as_one ? "Confirmed as one document" : "Each leg confirms on its own",
            as_one ? "Priced as a whole" : "Priced leg by leg"};
}

std::string
structure_routes::tree_json(const boost::uuids::uuid& structure_id,
                            const std::vector<trading_domain::structure>& structures,
                            const std::vector<trading_domain::structure_member>& members,
                            const std::map<std::string, std::string>& structure_references,
                            const std::map<std::string, std::string>& trade_references,
                            const std::map<std::string, trading_domain::trade>& trades,
                            const std::map<std::string, std::string>& type_names,
                            const std::map<std::string, std::string>& type_asset_classes) {
    using namespace structures;
    std::map<std::string, const trading_domain::structure*> by_id;
    for (const auto& row : structures)
        by_id.emplace(text_of(row.id), &row);
    const auto mine = by_id.find(text_of(structure_id));
    if (mine == by_id.end())
        return "null";

    /* The tenant's structures hold a parent as an id, so the root is found by
     * walking up rather than by being told. */
    auto root_id = mine->first;
    for (std::size_t step = 0; step < structures.size(); ++step) {
        const auto parent = text_of(by_id.at(root_id)->parent_structure_id);
        if (parent.empty() || by_id.find(parent) == by_id.end())
            break;
        root_id = parent;
    }

    /* A catalogue entry states its own name, and the ones that do are used as
     * they stand; the rest are codes a desk still has to read. */
    const auto product_of = [&](const std::string& type) {
        const auto found = type_names.find(type);
        const auto name = found == type_names.end() ? std::string() : found->second;
        return !name.empty() && name != type ? name : humanised(type);
    };

    std::function<std::string(const std::string&, const std::string&, int)> assemble =
        [&](const std::string& id, const std::string& role, int sequence) {
            const auto row = by_id.at(id);
            const auto reference = structure_references.find(id);
            std::vector<std::string> children;
            for (const auto& child : structures)
                if (text_of(child.parent_structure_id) == id && text_of(child.id) != id)
                    children.push_back(assemble(text_of(child.id), "structure", 0));
            for (const auto& member : members) {
                if (text_of(member.structure_id) != id)
                    continue;
                const auto trade_id = text_of(member.trade_id);
                const auto trade = trades.find(trade_id);
                const auto type = trade == trades.end() ? std::string() : trade->second.trade_type;
                const auto trade_reference = trade_references.find(trade_id);
                const auto home = type_asset_classes.find(type);
                const auto external_version = trade == trades.end() ?
                                                  std::string("null") :
                                                  std::to_string(trade->second.external_version);
                children.push_back(
                    "{\"id\":" + json::write(trade_id) + ",\"reference\":" +
                    json::write(trade_reference == trade_references.end() ?
                                    std::string() :
                                    trade_reference->second) +
                    ",\"kind\":" + json::write(type) +
                    ",\"template_code\":\"\",\"role\":" + json::write(member.role) +
                    ",\"sequence_number\":" + std::to_string(member.sequence_number) +
                    ",\"product\":" + json::write(product_of(type)) + ",\"asset_class\":" +
                    json::write(asset_class_named(
                        home == type_asset_classes.end() ? std::string() : home->second)) +
                    ",\"external_version\":" + external_version + ",\"children\":[]}");
            }
            return "{\"id\":" + json::write(id) + ",\"reference\":" +
                   json::write(reference == structure_references.end() ? std::string() :
                                                                         reference->second) +
                   ",\"kind\":" + json::write(row->kind) +
                   ",\"template_code\":" + json::write(row->template_code) +
                   ",\"role\":" + json::write(role) +
                   ",\"sequence_number\":" + std::to_string(sequence) + ",\"product\":\"\"" +
                   ",\"asset_class\":\"\",\"external_version\":null,\"children\":" +
                   rr::array_of(children) + "}";
        };
    return assemble(root_id, "", 0);
}

void structure_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace structures;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"trading"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/structure-kinds")
            .summary("The kinds a grouping may choose, and how each confirms")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_structure_kinds_request>([&] {
                    trading_messaging::list_structure_kinds_request msg;
                    msg.filter.emplace();
                    const auto rows = forwarding::forward_all(
                        req,
                        session,
                        msg,
                        &trading_messaging::list_structure_kinds_response::structure_kinds);
                    std::string out = "[";
                    for (const auto& row : rows) {
                        out += out.size() == 1 ? "{" : ",{";
                        out += "\"code\":" + json::write(row.code) +
                               ",\"confirms_as_whole\":" + json::write(row.confirms_as_whole) + "}";
                    }
                    return http_response::json("{\"rows\":" + out + "]}");
                });
            }));

    add(router->get("/api/ungrouped-trades")
            .summary("The trades no structure has grouped yet, as the picker shows them")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_trade_summaries_request>([&] {
                    auto [free, references] = unheld_trades(req, session);
                    std::vector<boost::uuids::uuid> counterparty_ids;
                    for (const auto& row : free)
                        if (row.counterparty_id &&
                            !std::ranges::contains(counterparty_ids, *row.counterparty_id))
                            counterparty_ids.push_back(*row.counterparty_id);
                    const auto names = counterparty_names(req, session, counterparty_ids);
                    std::vector<std::string> out;
                    for (const auto& row : free) {
                        const auto id = text_of(row.id);
                        const auto reference = references.find(id);
                        const auto counterparty_id = text_of(row.counterparty_id);
                        const auto name = names.find(counterparty_id);
                        out.push_back(
                            "{\"id\":" + json::write(id) + ",\"reference\":" +
                            json::write(reference == references.end() ? std::string() :
                                                                        reference->second) +
                            ",\"instrument_type\":" + json::write(row.instrument_type) +
                            ",\"counterparty_id\":" + json::write(counterparty_id) +
                            ",\"counterparty_name\":" +
                            json::write(name == names.end() ? counterparty_id : name->second) +
                            ",\"trade_date\":" +
                            (row.trade_date ? json::write(*row.trade_date) : std::string("\"\"")) +
                            "}");
                    }
                    return http_response::json("{\"rows\":" + rr::array_of(out) + "}");
                });
            }));

    add(router->post("/api/structures")
            .summary("Group trades into a structure")
            .description("One operation, not a loop: the completion writes the structure, its "
                         "members and takes every leg live in one transaction, so a refusal "
                         "leaves every leg a draft and writes no member.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::complete_structure_request>([&] {
                    const std::string sentence =
                        "A grouping states a kind of 1 to 64 characters and 2 to 50 trades, each "
                        "named by its id.";
                    rr::refuse_unknown_members(req.body, {"kind", "tradeIds"}, sentence);
                    const auto body = rr::read_body<group_body>(req.body);
                    if (!body)
                        rr::invalid(sentence);
                    const auto kind = rr::trimmed(body->kind);
                    if (kind.empty() || rr::characters_of(kind) > 64 || body->tradeIds.size() < 2 ||
                        body->tradeIds.size() > 50)
                        rr::invalid(sentence);
                    std::vector<boost::uuids::uuid> trade_ids;
                    for (const auto& text : body->tradeIds) {
                        if (!rr::is_uuid(text))
                            rr::invalid(sentence);
                        trade_ids.push_back(boost::uuids::string_generator()(text));
                    }
                    if (distinct(trade_ids).size() != trade_ids.size())
                        rr::invalid("A trade appears twice in the grouping.");

                    const auto party = req.authenticated_user ? req.authenticated_user->party_id :
                                                                std::optional<std::string>();
                    if (!party || party->empty())
                        rr::invalid("A grouping is made by a party, and this session works for "
                                    "none.");

                    const auto summaries = all_summaries(req, session);
                    std::vector<boost::uuids::uuid> counterparties;
                    for (const auto& id : trade_ids) {
                        const auto found = std::ranges::find_if(
                            summaries,
                            [&](const trading_domain::trade_summary& row) { return row.id == id; });
                        if (found == summaries.end())
                            rr::invalid("A trade being grouped is not one this session can read.");
                        if (found->counterparty_id &&
                            !std::ranges::contains(counterparties, *found->counterparty_id))
                            counterparties.push_back(*found->counterparty_id);
                    }
                    if (counterparties.size() != 1)
                        rr::invalid("Every trade in a structure shares one counterparty, and "
                                    "these do not.");

                    trading_messaging::list_structure_kinds_request kinds_msg;
                    kinds_msg.filter.emplace();
                    const auto kinds = forwarding::forward_all(
                        req,
                        session,
                        kinds_msg,
                        &trading_messaging::list_structure_kinds_response::structure_kinds);
                    const auto known =
                        std::ranges::find_if(kinds, [&](const trading_domain::structure_kind& row) {
                            return row.code == kind;
                        });
                    if (known == kinds.end())
                        rr::invalid("The tenant has no structure kind with that code.");

                    const auto members = all_members(req, session);
                    for (const auto& member : members)
                        if (std::ranges::find(trade_ids, member.trade_id) != trade_ids.end())
                            rr::refuse(http_status::conflict,
                                       "conflict",
                                       "A trade in the grouping is already a leg of a structure.");

                    const auto now = std::chrono::system_clock::now();
                    const auto structure_id = boost::uuids::random_generator()();
                    trading_messaging::complete_structure_request msg;
                    msg.structure.version = 0;
                    msg.structure.tenant_id = ores::utility::uuid::tenant_id::system();
                    msg.structure.id = structure_id;
                    msg.structure.party_id = boost::uuids::string_generator()(*party);
                    msg.structure.counterparty_id = counterparties.front();
                    msg.structure.kind = kind;
                    msg.structure.template_code.clear();
                    msg.structure.parent_structure_id = no_parent;
                    msg.structure.change_reason_code = std::string(new_record);
                    msg.structure.change_commentary = std::string(grouped_note);
                    msg.structure.recorded_at = now;
                    int sequence = 0;
                    for (const auto& trade_id : trade_ids) {
                        trading_domain::structure_member leg;
                        leg.version = 0;
                        leg.tenant_id = ores::utility::uuid::tenant_id::system();
                        leg.trade_id = trade_id;
                        leg.structure_id = structure_id;
                        leg.role = std::string(free_role);
                        leg.sequence_number = ++sequence;
                        leg.change_reason_code = std::string(new_record);
                        leg.recorded_at = now;
                        msg.legs.push_back(leg);
                    }
                    msg.leg_count = static_cast<int>(msg.legs.size());
                    msg.change_reason_code = std::string(new_record);
                    msg.change_commentary = std::string(grouped_note);

                    const auto completed = forwarding::forward(req, session, msg);
                    rr::require_ok(completed.result);
                    const auto references =
                        structure_references(req, session, std::vector{structure_id});
                    const auto reference = references.find(text_of(structure_id));
                    return http_response::json(
                        "{\"id\":" + json::write(structure_id) + ",\"reference\":" +
                            json::write(reference == references.end() ? std::string() :
                                                                        reference->second) +
                            "}",
                        http_status::created);
                });
            }));

    add(router->get("/api/structures")
            .summary("One page of the structures a tenant holds, as a tree flattened for reading")
            .description("A root row, then its children indented under it. Each row states what "
                         "it holds: the trades inside it and the structures under it. A search "
                         "keeps the rows it matches, everything under them and the path above "
                         "them, because a tree with a hole in it is not readable.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_structures_request>([&] {
                    const std::string sentence =
                        "A page of structures names an offset of zero or more, a limit of 1 to "
                        "200, a search of up to 256 characters and a kind of up to 64.";
                    rr::refuse_unknown_params(req, {"offset", "limit", "search", "kind"});
                    const auto whole =
                        [&](const std::string& name, int lowest, int highest, int fallback) {
                            if (!req.query_params.contains(name))
                                return fallback;
                            const auto raw = rr::trimmed(req.get_query_param(name));
                            int value = 0;
                            const auto [end, error] =
                                std::from_chars(raw.data(), raw.data() + raw.size(), value);
                            if (raw.empty() || error != std::errc{} ||
                                end != raw.data() + raw.size() || value < lowest || value > highest)
                                rr::invalid(sentence);
                            return value;
                        };
                    const auto offset = whole("offset", 0, 2147483647, 0);
                    const auto limit = whole("limit", 1, 200, 25);
                    const auto search = req.query_params.contains("search") ?
                                            rr::trimmed(req.get_query_param("search")) :
                                            std::string();
                    const auto kind = req.query_params.contains("kind") ?
                                          rr::trimmed(req.get_query_param("kind")) :
                                          std::string();
                    if (rr::characters_of(search) > 256 || rr::characters_of(kind) > 64)
                        rr::invalid(sentence);

                    const auto structures = all_structures(req, session);
                    const auto members = all_members(req, session);
                    std::vector<boost::uuids::uuid> structure_ids;
                    for (const auto& row : structures)
                        structure_ids.push_back(row.id);
                    const auto references = structure_references(req, session, structure_ids);
                    std::vector<boost::uuids::uuid> counterparty_ids;
                    for (const auto& row : structures)
                        if (!std::ranges::contains(counterparty_ids, row.counterparty_id))
                            counterparty_ids.push_back(row.counterparty_id);
                    const auto names = counterparty_names(req, session, counterparty_ids);

                    std::map<std::string, const trading_domain::structure*> by_id;
                    for (const auto& row : structures)
                        by_id.emplace(text_of(row.id), &row);
                    const auto parent_of = [&](const std::string& id) {
                        const auto found = by_id.find(id);
                        if (found == by_id.end())
                            return std::string();
                        const auto parent = text_of(found->second->parent_structure_id);
                        return parent == text_of(no_parent) ? std::string() : parent;
                    };
                    const auto root_of = [&](const std::string& id) {
                        auto current = id;
                        for (std::size_t step = 0; step < structures.size(); ++step) {
                            const auto parent = parent_of(current);
                            if (parent.empty() || by_id.find(parent) == by_id.end())
                                return current;
                            current = parent;
                        }
                        return current;
                    };
                    const auto depth_of = [&](const std::string& id) {
                        auto current = parent_of(id);
                        int depth = 0;
                        while (!current.empty() && by_id.find(current) != by_id.end() &&
                               depth < static_cast<int>(structures.size())) {
                            ++depth;
                            current = parent_of(current);
                        }
                        return depth;
                    };

                    std::vector<list_row> rows;
                    rows.reserve(structures.size());
                    for (const auto& entry : structures) {
                        const auto id = text_of(entry.id);
                        const auto reference = references.find(id);
                        const auto name = names.find(text_of(entry.counterparty_id));
                        const auto verdict = verdicts_of(entry.kind);
                        list_row row;
                        row.id = id;
                        row.reference =
                            reference == references.end() ? std::string() : reference->second;
                        row.kind = entry.kind;
                        row.template_code = entry.template_code;
                        row.counterparty_id = text_of(entry.counterparty_id);
                        row.counterparty_name = name == names.end() ? std::string() : name->second;
                        row.legs = static_cast<std::size_t>(std::ranges::count_if(
                            members, [&](const trading_domain::structure_member& member) {
                                return text_of(member.structure_id) == id;
                            }));
                        row.inner_structures = static_cast<std::size_t>(std::ranges::count_if(
                            structures, [&](const trading_domain::structure& other) {
                                return parent_of(text_of(other.id)) == id;
                            }));
                        row.confirms = verdict.confirms;
                        row.prices = verdict.prices;
                        row.parent_id = parent_of(id);
                        row.depth = depth_of(id);
                        rows.push_back(std::move(row));
                    }

                    /* A search keeps the rows it matches, everything under them
                     * and the path above them. */
                    std::string needle = search;
                    std::transform(
                        needle.begin(), needle.end(), needle.begin(), [](unsigned char c) {
                            return static_cast<char>(std::tolower(c));
                        });
                    const auto matches = [&](const list_row& row) {
                        if (!kind.empty() && row.kind != kind)
                            return false;
                        if (needle.empty())
                            return true;
                        auto haystack = row.reference + " " + row.kind + " " + row.template_code +
                                        " " + row.counterparty_name;
                        std::transform(
                            haystack.begin(),
                            haystack.end(),
                            haystack.begin(),
                            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
                        return haystack.find(needle) != std::string::npos;
                    };

                    std::vector<std::string> visible;
                    if (needle.empty() && kind.empty()) {
                        for (const auto& row : rows)
                            visible.push_back(row.id);
                    } else {
                        std::function<void(const std::string&)> add_under =
                            [&](const std::string& id) {
                                if (std::ranges::find(visible, id) == visible.end())
                                    visible.push_back(id);
                                for (const auto& child : rows)
                                    if (child.parent_id == id)
                                        add_under(child.id);
                            };
                        for (const auto& row : rows) {
                            if (!matches(row))
                                continue;
                            add_under(row.id);
                            auto above = row.parent_id;
                            while (!above.empty() && by_id.find(above) != by_id.end()) {
                                if (std::ranges::find(visible, above) == visible.end())
                                    visible.push_back(above);
                                above = parent_of(above);
                            }
                        }
                    }

                    std::vector<const list_row*> ordered;
                    for (const auto& row : rows)
                        if (std::ranges::find(visible, row.id) != visible.end())
                            ordered.push_back(&row);
                    std::stable_sort(
                        ordered.begin(),
                        ordered.end(),
                        [&](const list_row* left, const list_row* right) {
                            const auto left_root = root_of(left->id);
                            const auto right_root = root_of(right->id);
                            if (left_root != right_root) {
                                const auto left_reference =
                                    references.find(left_root) == references.end() ?
                                        left_root :
                                        references.at(left_root);
                                const auto right_reference =
                                    references.find(right_root) == references.end() ?
                                        right_root :
                                        references.at(right_root);
                                std::string lowered_left = left_reference;
                                std::string lowered_right = right_reference;
                                std::transform(lowered_left.begin(),
                                               lowered_left.end(),
                                               lowered_left.begin(),
                                               [](unsigned char c) {
                                                   return static_cast<char>(std::tolower(c));
                                               });
                                std::transform(lowered_right.begin(),
                                               lowered_right.end(),
                                               lowered_right.begin(),
                                               [](unsigned char c) {
                                                   return static_cast<char>(std::tolower(c));
                                               });
                                return lowered_left < lowered_right;
                            }
                            return left->depth < right->depth;
                        });

                    std::vector<std::string> kinds;
                    for (const auto& row : rows)
                        if (!row.kind.empty() && std::ranges::find(kinds, row.kind) == kinds.end())
                            kinds.push_back(row.kind);
                    std::sort(kinds.begin(), kinds.end());

                    std::vector<std::string> page;
                    const auto first = static_cast<std::size_t>(offset);
                    for (std::size_t i = first;
                         i < ordered.size() && i < first + static_cast<std::size_t>(limit);
                         ++i) {
                        const auto& row = *ordered[i];
                        page.push_back(
                            "{\"id\":" + json::write(row.id) + ",\"reference\":" +
                            json::write(row.reference) + ",\"kind\":" + json::write(row.kind) +
                            ",\"template_code\":" + json::write(row.template_code) +
                            ",\"counterparty_id\":" + json::write(row.counterparty_id) +
                            ",\"counterparty_name\":" + json::write(row.counterparty_name) +
                            ",\"legs\":" + std::to_string(row.legs) +
                            ",\"inner_structures\":" + std::to_string(row.inner_structures) +
                            ",\"confirms\":" + json::write(row.confirms) +
                            ",\"prices\":" + json::write(row.prices) +
                            ",\"parent_id\":" + json::write(row.parent_id) +
                            ",\"depth\":" + std::to_string(row.depth) + "}");
                    }
                    return http_response::json("{\"rows\":" + rr::array_of(page) +
                                               ",\"total\":" + std::to_string(ordered.size()) +
                                               ",\"kinds\":" + rr::array_of(kinds) + "}");
                });
            }));

    add(router->get("/api/structures/{key}")
            .summary("One structure's page: the tree it sits in, its legs and its party")
            .description("The key is the reference the desk knows or the deal's id, so a link "
                         "can carry the reference a person reads. The party is read from the "
                         "legs, because a structure holds none of its own.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_structures_request>([&] {
                    const auto structure = resolve_structure(req, session, key_of(req));
                    const auto structure_id = text_of(structure.id);
                    const auto structures = all_structures(req, session);
                    const auto members = all_members(req, session);
                    std::vector<boost::uuids::uuid> mine;
                    std::vector<boost::uuids::uuid> all_trade_ids;
                    for (const auto& member : members) {
                        if (text_of(member.structure_id) == structure_id)
                            mine.push_back(member.trade_id);
                        if (!std::ranges::contains(all_trade_ids, member.trade_id))
                            all_trade_ids.push_back(member.trade_id);
                    }
                    std::vector<boost::uuids::uuid> structure_ids;
                    for (const auto& row : structures)
                        structure_ids.push_back(row.id);
                    const auto structure_refs = structure_references(req, session, structure_ids);
                    const auto trade_refs = trade_references(req, session, all_trade_ids);
                    const auto trades = trades_by_ids(req, session, all_trade_ids);
                    const auto named = catalogue_of(req, session);
                    std::map<std::string, std::string> states;
                    try {
                        states = states_of(req, session, all_trade_ids);
                    } catch (const rr::refusal&) {
                        /* A caller who may read the deal and not the trade list
                         * still gets the deal, with no state drawn. */
                    }

                    std::vector<const trading_domain::structure_member*> legs;
                    for (const auto& member : members)
                        if (text_of(member.structure_id) == structure_id)
                            legs.push_back(&member);
                    std::stable_sort(
                        legs.begin(), legs.end(), [](const auto* left, const auto* right) {
                            return left->sequence_number < right->sequence_number;
                        });

                    std::vector<std::string> legs_json;
                    std::vector<std::string> party_ids;
                    for (const auto* member : legs) {
                        const auto trade_id = text_of(member->trade_id);
                        const auto found = trades.find(trade_id);
                        const auto type =
                            found == trades.end() ? std::string() : found->second.trade_type;
                        const auto reference = trade_refs.find(trade_id);
                        const auto state = states.find(trade_id);
                        if (found != trades.end()) {
                            const auto party = text_of(found->second.party_id);
                            if (!party.empty() && !std::ranges::contains(party_ids, party))
                                party_ids.push_back(party);
                        }
                        legs_json.push_back(
                            "{\"trade_id\":" + json::write(trade_id) + ",\"reference\":" +
                            json::write(reference == trade_refs.end() ? std::string() :
                                                                        reference->second) +
                            ",\"role\":" + json::write(member->role) +
                            ",\"sequence_number\":" + std::to_string(member->sequence_number) +
                            ",\"version\":" + std::to_string(member->version) +
                            ",\"kind\":" + json::write(type) +
                            ",\"product\":" + json::write(name_of(named, type)) +
                            ",\"asset_class\":" + json::write(class_of(named, type)) +
                            ",\"state\":" +
                            json::write(state == states.end() ? std::string() : state->second) +
                            ",\"external_version\":" +
                            (found == trades.end() ?
                                 std::string("null") :
                                 std::to_string(found->second.external_version)) +
                            "}");
                    }

                    /* A structure holds no party of its own: every leg is booked
                     * as the same party, which the database enforces. */
                    const auto party_id = party_ids.empty() ? std::string() : party_ids.front();
                    const auto verdict = verdicts_of(structure.kind);
                    const auto counterparty_id = text_of(structure.counterparty_id);
                    const auto counterparties =
                        counterparty_id.empty() ?
                            std::map<std::string, std::string>() :
                            counterparty_names(
                                req, session, std::vector{structure.counterparty_id});
                    const auto named_counterparty = counterparties.find(counterparty_id);
                    const auto parent = text_of(structure.parent_structure_id);
                    const auto written = rr::object_with_members(
                        json::write(structure),
                        {{"counterparty_name",
                          json::write(named_counterparty == counterparties.end() ||
                                              named_counterparty->second.empty() ?
                                          counterparty_id :
                                          named_counterparty->second)}});
                    return http_response::json(
                        "{\"structure\":" + written + ",\"reference\":" +
                        json::write(structure_refs.contains(structure_id) ?
                                        structure_refs.at(structure_id) :
                                        std::string()) +
                        ",\"party\":{\"id\":" + json::write(party_id) +
                        ",\"name\":\"\",\"shared\":" + json::write(party_ids.size() <= 1) +
                        "},\"confirms\":" + json::write(verdict.confirms) +
                        ",\"prices\":" + json::write(verdict.prices) + ",\"parent_id\":" +
                        json::write(parent == text_of(no_parent) ? std::string() : parent) +
                        ",\"tree\":" +
                        tree_json(structure.id,
                                  structures,
                                  members,
                                  structure_refs,
                                  trade_refs,
                                  trades,
                                  named.names,
                                  named.classes) +
                        ",\"legs\":" + rr::array_of(legs_json) + ",\"roles\":[\"" +
                        std::string(free_role) + "\"]}");
                });
            }));

    add(router->delete_("/api/structures/{key}/legs/{tradeId}")
            .summary("Take a trade out of a structure")
            .description("The leg is removed, not the trade: the trade stays booked and becomes "
                         "groupable again. The key is the trade and the trade alone, so the route "
                         "checks the leg belongs to the structure it names before writing.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::delete_structure_member_request>([&] {
                    const auto why = unlink_reason_of(req);
                    const auto structure = resolve_structure(req, session, key_of(req));
                    const auto structure_id = text_of(structure.id);
                    const auto raw_trade = req.get_path_param("tradeId");
                    if (!rr::is_uuid(raw_trade))
                        rr::invalid("The address does not name a trade.");
                    const auto trade_id = boost::uuids::string_generator()(raw_trade);
                    trading_messaging::list_structure_members_request held_msg;
                    held_msg.filter.emplace();
                    held_msg.filter->trade_id_one_of = std::vector{trade_id};
                    const auto held = forwarding::forward_all(
                        req,
                        session,
                        held_msg,
                        &trading_messaging::list_structure_members_response::members);
                    if (held.empty() || text_of(held.front().structure_id) != structure_id)
                        rr::refuse(http_status::conflict,
                                   "conflict",
                                   "That trade is not a leg of this structure.");
                    trading_messaging::delete_structure_member_request msg;
                    msg.removal.key.trade_id = trade_id;
                    msg.removal.precondition.kind = ores::utility::domain::precondition_kind::any;
                    msg.intent = {std::string(amend_record), why};
                    const auto written = forwarding::forward(req, session, msg);
                    rr::require_ok(written.result);
                    return rr::no_content();
                });
            }));

    add(router->get("/api/structures/{key}/leg-candidates")
            .summary("The trades the structure may take as a leg")
            .description("Every trade no structure holds yet, newest first.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::list_trade_summaries_request>([&] {
                    const auto structure = resolve_structure(req, session, key_of(req));
                    auto [free, references] = unheld_trades(req, session);
                    std::vector<std::string> out;
                    for (const auto& row : free) {
                        const auto id = text_of(row.id);
                        const auto reference = references.find(id);
                        out.push_back(
                            "{\"id\":" + json::write(id) + ",\"reference\":" +
                            json::write(reference == references.end() ? std::string() :
                                                                        reference->second) +
                            ",\"instrument_type\":" + json::write(row.instrument_type) + "}");
                    }
                    return http_response::json(
                        "{\"structure_id\":" + json::write(text_of(structure.id)) +
                        ",\"rows\":" + rr::array_of(out) + "}");
                });
            }));

    add(router->post("/api/structures/{key}/legs")
            .summary("Put a trade into a structure as a leg")
            .description("A trade is a leg of one structure at a time, which the store states by "
                         "keying members on the trade. The route checks it so the screen is told "
                         "which structure already holds it rather than a bare conflict.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<trading_messaging::put_structure_member_request>([&] {
                    auto msg = member_of(req);
                    const auto structure = resolve_structure(req, session, key_of(req));
                    const auto structure_id = text_of(structure.id);
                    const auto trade_id = msg.change.write.trade_id;
                    trading_messaging::list_structure_members_request held_msg;
                    held_msg.filter.emplace();
                    held_msg.filter->trade_id_one_of = std::vector{trade_id};
                    const auto held = forwarding::forward_all(
                        req,
                        session,
                        held_msg,
                        &trading_messaging::list_structure_members_response::members);
                    if (!held.empty()) {
                        const auto holder = text_of(held.front().structure_id);
                        rr::refuse(http_status::conflict,
                                   "conflict",
                                   holder == structure_id ?
                                       "The trade is already a leg of this structure." :
                                       "The trade is a leg of another structure.");
                    }
                    const auto members = all_members(req, session);
                    int sequence = 0;
                    for (const auto& member : members)
                        if (text_of(member.structure_id) == structure_id)
                            sequence = std::max(sequence, member.sequence_number);
                    msg.change.write.structure_id = structure.id;
                    msg.change.write.sequence_number = sequence + 1;
                    const auto written = forwarding::forward(req, session, msg);
                    rr::require_ok(written.result);
                    return http_response::json(
                        "{\"tradeId\":" + json::write(trade_id) + ",\"role\":" +
                            json::write(msg.change.write.role) + ",\"sequenceNumber\":" +
                            std::to_string(msg.change.write.sequence_number) + "}",
                        http_status::created);
                });
            }));
}

}
