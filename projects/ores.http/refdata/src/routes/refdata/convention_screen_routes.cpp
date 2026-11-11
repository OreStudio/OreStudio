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
#include "ores.http/routes/refdata/convention_screen_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.api/messaging/average_ois_convention_protocol.hpp"
#include "ores.refdata.api/messaging/bma_basis_swap_convention_protocol.hpp"
#include "ores.refdata.api/messaging/bond_yield_convention_protocol.hpp"
#include "ores.refdata.api/messaging/cds_convention_protocol.hpp"
#include "ores.refdata.api/messaging/cms_spread_option_convention_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_forward_convention_protocol.hpp"
#include "ores.refdata.api/messaging/commodity_future_convention_protocol.hpp"
#include "ores.refdata.api/messaging/convention_kind_protocol.hpp"
#include "ores.refdata.api/messaging/convention_protocol.hpp"
#include "ores.refdata.api/messaging/cross_currency_basis_convention_protocol.hpp"
#include "ores.refdata.api/messaging/cross_currency_fix_float_convention_protocol.hpp"
#include "ores.refdata.api/messaging/currency_pair_convention_protocol.hpp"
#include "ores.refdata.api/messaging/deposit_convention_protocol.hpp"
#include "ores.refdata.api/messaging/fra_convention_protocol.hpp"
#include "ores.refdata.api/messaging/future_convention_protocol.hpp"
#include "ores.refdata.api/messaging/fx_option_convention_protocol.hpp"
#include "ores.refdata.api/messaging/ibor_index_convention_protocol.hpp"
#include "ores.refdata.api/messaging/inflation_swap_convention_protocol.hpp"
#include "ores.refdata.api/messaging/intraday_power_load_convention_protocol.hpp"
#include "ores.refdata.api/messaging/ois_convention_protocol.hpp"
#include "ores.refdata.api/messaging/overnight_index_convention_protocol.hpp"
#include "ores.refdata.api/messaging/swap_convention_protocol.hpp"
#include "ores.refdata.api/messaging/swap_index_convention_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_convention_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_basis_two_swap_convention_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_convention_protocol.hpp"
#include "ores.refdata.api/messaging/zero_convention_protocol.hpp"
#include "ores.refdata.api/messaging/zero_inflation_index_convention_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstdint>
#include <cstdlib>
#include <limits>
#include <map>
#include <optional>
#include <ranges>
#include <set>
#include <tuple>
#include <type_traits>
#include <yyjson.h>

namespace ores::http::routes::refdata {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;
namespace utility_domain = ores::utility::domain;

namespace {

/// The most rows one page of a family may hold, as the BFF allowed.
constexpr std::int64_t convention_screen_most_rows = 500;

/// A page of a family's rows as JSON, and how many rows the family holds.
struct listed final {
    std::string rows = "[]";
    std::uint64_t total = 0;
};

struct unit;

using page_fn = listed (*)(
    const http_request&, nats_client&, const unit&, std::uint32_t offset, std::uint32_t limit);

using write_fn = std::string (*)(const http_request&, nats_client&, const std::string& body);

/**
 * One family the screen lists, and how its rows are read and written.
 *
 * kind_code is set only for a composite family. A family with no write is one
 * whose terms the screen does not draw yet.
 */
struct unit final {
    std::string_view key;
    std::string_view entity;
    std::string_view kind_code;
    page_fn page;
    write_fn write;
};

/// A family's page through its own list.
template <typename List, auto Rows>
listed plain_page(const http_request& req,
                  nats_client& session,
                  const unit&,
                  std::uint32_t offset,
                  std::uint32_t limit) {
    List msg;
    msg.offset = offset;
    msg.limit = limit;
    const auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return {json::write(reply.*Rows), reply.total};
}

/// The central rows of a composite family: the convention entity filtered by its kind.
messaging::list_conventions_response kind_page(const http_request& req,
                                               nats_client& session,
                                               std::string_view kind_code,
                                               std::uint32_t offset,
                                               std::uint32_t limit) {
    messaging::list_conventions_request msg;
    msg.offset = offset;
    msg.limit = limit;
    msg.filter = messaging::conventions_filter{};
    msg.filter->kind_code = std::string(kind_code);
    auto reply = ores::http::forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return reply;
}

/**
 * A composite family's page: the central rows, each with the parts the
 * family's get_many answers for exactly that page's keys laid over it.
 */
template <typename GetMany>
listed composite_page(const http_request& req,
                      nats_client& session,
                      const unit& family,
                      std::uint32_t offset,
                      std::uint32_t limit) {
    const auto central = kind_page(req, session, family.kind_code, offset, limit);
    listed out;
    out.total = central.total;
    if (central.conventions.empty())
        return out;

    GetMany many;
    for (const auto& row : central.conventions) {
        typename decltype(many.keys)::value_type key;
        key.id = row.identity.id;
        many.keys.push_back(key);
    }
    const auto parts = ores::http::forwarding::forward(req, session, many);
    rr::require_ok(parts.result);
    std::map<std::string, std::string> item_of;
    for (const auto& item : parts.items)
        item_of[boost::uuids::to_string(item.key.id)] = json::write(item);

    out.rows = "[";
    for (const auto& row : central.conventions) {
        auto flat = convention_screen_routes::flatten_central(json::write(row));
        const auto found = item_of.find(boost::uuids::to_string(row.identity.id));
        if (found != item_of.end())
            flat = convention_screen_routes::zip_parts(flat, found->second);
        out.rows += (out.rows.size() == 1 ? "" : ",") + flat;
    }
    out.rows += "]";
    return out;
}

/// The bodies the screen sends, beside the wire_fields each is read with.
namespace convention_screen {

/// What a family write states: why, the version the row was read at, and the row.
template <typename Write>
struct family_write_body final {
    rr::stated_intent intent;
    std::optional<std::uint32_t> version;
    Write write;
};

template <typename Write>
[[nodiscard]] constexpr auto wire_fields(std::type_identity<family_write_body<Write>>) {
    using ores::utility::codec::field;
    return std::tuple{field{"intent", &family_write_body<Write>::intent},
                      field{"version", &family_write_body<Write>::version},
                      field{"write", &family_write_body<Write>::write}};
}

}

/**
 * A family's write. A row read at no version is new, and one read at a
 * version is written only while it still holds it. The service's result is
 * answered whatever its outcome, so the screen shows the field it refused.
 */
template <typename Put, auto Written>
std::string family_write(const http_request& req, nats_client& session, const std::string& text) {
    using write_type = decltype(Put{}.change.write);
    const auto body = rr::read_body<convention_screen::family_write_body<write_type>>(text);
    const auto intent = body ? rr::intent_of(body->intent) : std::nullopt;
    if (!body || !intent || !rr::is_uuid(body->write.id))
        rr::invalid("A convention write names the row by its identifier, the version it was "
                    "read at or null for a new row, and a reason.");
    Put msg;
    msg.change.write = body->write;
    msg.change.precondition.kind = body->version ?
                                       utility_domain::precondition_kind::must_match_version :
                                       utility_domain::precondition_kind::must_not_exist;
    msg.change.precondition.version = body->version;
    msg.intent = *intent;
    const auto reply = ores::http::forwarding::forward(req, session, msg);
    const auto& row = reply.*Written;
    return "{\"result\":" + json::write(reply.result) +
           ",\"convention\":" + (row ? json::write(*row) : std::string("null")) + "}";
}

/// A plain family that the screen lists and does not write.
template <typename List, auto Rows>
constexpr unit plain(std::string_view key, std::string_view entity) {
    return {key, entity, "", &plain_page<List, Rows>, nullptr};
}

/**
 * Every family the screen lists. A plain family comes from the convention
 * registry; a registry family a composite stands for is listed only through
 * that composite. The tenor and currency pair conventions are not in the
 * registry, because the conventions document does not carry them, but the
 * screen lists them.
 */
const unit units[] = {
    plain<messaging::list_average_ois_conventions_request,
          &messaging::list_average_ois_conventions_response::average_ois_conventions>(
        "average-ois", "average_ois_convention"),
    plain<messaging::list_bma_basis_swap_conventions_request,
          &messaging::list_bma_basis_swap_conventions_response::bma_basis_swap_conventions>(
        "bma-basis-swap", "bma_basis_swap_convention"),
    plain<messaging::list_bond_yield_conventions_request,
          &messaging::list_bond_yield_conventions_response::bond_yield_conventions>(
        "bond-yield", "bond_yield_convention"),
    plain<messaging::list_cds_conventions_request,
          &messaging::list_cds_conventions_response::cds_conventions>("cds", "cds_convention"),
    plain<messaging::list_cms_spread_option_conventions_request,
          &messaging::list_cms_spread_option_conventions_response::cms_spread_option_conventions>(
        "cms-spread-option", "cms_spread_option_convention"),
    plain<messaging::list_commodity_forward_conventions_request,
          &messaging::list_commodity_forward_conventions_response::commodity_forward_conventions>(
        "commodity-forward", "commodity_forward_convention"),
    {"deposit",
     "deposit_convention",
     "",
     &plain_page<messaging::list_deposit_conventions_request,
                 &messaging::list_deposit_conventions_response::deposit_conventions>,
     &family_write<messaging::put_deposit_convention_request,
                   &messaging::put_deposit_convention_response::deposit_convention>},
    plain<messaging::list_fra_conventions_request,
          &messaging::list_fra_conventions_response::fra_conventions>("fra", "fra_convention"),
    plain<messaging::list_future_conventions_request,
          &messaging::list_future_conventions_response::future_conventions>("future",
                                                                            "future_convention"),
    plain<messaging::list_fx_option_conventions_request,
          &messaging::list_fx_option_conventions_response::fx_option_conventions>(
        "fx-option", "fx_option_convention"),
    plain<messaging::list_ibor_index_conventions_request,
          &messaging::list_ibor_index_conventions_response::ibor_index_conventions>(
        "ibor-index", "ibor_index_convention"),
    plain<messaging::list_inflation_swap_conventions_request,
          &messaging::list_inflation_swap_conventions_response::inflation_swap_conventions>(
        "inflation-swap", "inflation_swap_convention"),
    plain<
        messaging::list_intraday_power_load_conventions_request,
        &messaging::list_intraday_power_load_conventions_response::intraday_power_load_conventions>(
        "intraday-power-load", "intraday_power_load_convention"),
    plain<messaging::list_ois_conventions_request,
          &messaging::list_ois_conventions_response::ois_conventions>("ois", "ois_convention"),
    plain<messaging::list_overnight_index_conventions_request,
          &messaging::list_overnight_index_conventions_response::overnight_index_conventions>(
        "overnight-index", "overnight_index_convention"),
    {"swap",
     "swap_convention",
     "",
     &plain_page<messaging::list_swap_conventions_request,
                 &messaging::list_swap_conventions_response::swap_conventions>,
     &family_write<messaging::put_swap_convention_request,
                   &messaging::put_swap_convention_response::swap_convention>},
    plain<messaging::list_swap_index_conventions_request,
          &messaging::list_swap_index_conventions_response::swap_index_conventions>(
        "swap-index", "swap_index_convention"),
    plain<messaging::list_tenor_basis_two_swap_conventions_request,
          &messaging::list_tenor_basis_two_swap_conventions_response::
              tenor_basis_two_swap_conventions>("tenor-basis-two-swap",
                                                "tenor_basis_two_swap_convention"),
    plain<messaging::list_zero_conventions_request,
          &messaging::list_zero_conventions_response::zero_conventions>("zero", "zero_convention"),
    plain<messaging::list_zero_inflation_index_conventions_request,
          &messaging::list_zero_inflation_index_conventions_response::
              zero_inflation_index_conventions>("zero-inflation-index",
                                                "zero_inflation_index_convention"),
    {"commodity-future",
     "commodity_future_convention",
     "COMMODITY_FUTURE",
     &composite_page<messaging::get_many_commodity_future_conventions_request>,
     nullptr},
    {"cross-currency-basis",
     "cross_currency_basis_convention",
     "CROSS_CURRENCY_BASIS",
     &composite_page<messaging::get_many_cross_currency_basis_conventions_request>,
     nullptr},
    {"cross-currency-fix-float",
     "cross_currency_fix_float_convention",
     "CROSS_CURRENCY_FIX_FLOAT",
     &composite_page<messaging::get_many_cross_currency_fix_float_conventions_request>,
     nullptr},
    {"tenor-basis-swap",
     "tenor_basis_swap_convention",
     "TENOR_BASIS_SWAP",
     &composite_page<messaging::get_many_tenor_basis_swap_conventions_request>,
     nullptr},
    {"tenor",
     "tenor_convention",
     "",
     &plain_page<messaging::list_tenor_conventions_request,
                 &messaging::list_tenor_conventions_response::conventions>,
     nullptr},
    {"currency-pair",
     "currency_pair_convention",
     "",
     &plain_page<messaging::list_currency_pair_conventions_request,
                 &messaging::list_currency_pair_conventions_response::conventions>,
     nullptr},
};

/**
 * Whether the tenant holds a family once for every party. A composite is held
 * per party. A plain family is shared when the registry says its table has no
 * party. The tenor and currency pair conventions are held once by the tenant.
 */
bool shared_of(const unit& family) {
    if (!family.kind_code.empty())
        return false;
    for (const auto& entry : ores::refdata::domain::convention_families)
        if (entry.entity == family.entity)
            return !entry.party_scoped;
    return true;
}

const unit* unit_of(std::string_view key) {
    const auto found = std::ranges::find(units, key, &unit::key);
    return found == std::ranges::end(units) ? nullptr : &*found;
}

/// The family the address names, or a refusal when it names none.
const unit& family_of(const http_request& req) {
    const auto key = req.get_path_param("family");
    const auto* found = unit_of(key);
    if (found == nullptr)
        rr::refuse(
            http_status::not_found, "not-found", "There is no convention family " + key + ".");
    return *found;
}

/// The kind code a family stands for: tenor-basis-swap is TENOR_BASIS_SWAP.
std::string kind_code_of(std::string_view key) {
    std::string out;
    for (const char c : key)
        out += c == '-' ? '_' : static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
    return out;
}

/// How many rows a family holds, or nothing when the count cannot be read.
std::optional<std::uint64_t>
count_of(const http_request& req, nats_client& session, const unit& family) {
    try {
        if (family.kind_code.empty())
            return family.page(req, session, family, 0, 1).total;
        return kind_page(req, session, family.kind_code, 0, 1).total;
    } catch (const rr::refusal&) {
        return std::nullopt;
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

/// A whole number query parameter in a range, its default when left out, or a refusal.
std::uint32_t bounded(const http_request& req,
                      const std::string& name,
                      std::int64_t lowest,
                      std::int64_t highest,
                      std::int64_t fallback) {
    const auto raw = req.get_query_param(name);
    if (raw.empty())
        return static_cast<std::uint32_t>(fallback);
    std::int64_t value = 0;
    const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), value);
    if (error != std::errc{} || end != raw.data() + raw.size() || value < lowest || value > highest)
        rr::invalid("The " + name + " is a whole number from " + std::to_string(lowest) + " to " +
                    std::to_string(highest) + ".");
    return static_cast<std::uint32_t>(value);
}

using doc_ptr = std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)>;
using mut_doc_ptr = std::unique_ptr<yyjson_mut_doc, decltype(&yyjson_mut_doc_free)>;

doc_ptr read_json(std::string_view text) {
    return {yyjson_read(text.data(), text.size(), 0), &yyjson_doc_free};
}

yyjson_val* root_object(const doc_ptr& doc) {
    auto* root = doc ? yyjson_doc_get_root(doc.get()) : nullptr;
    return yyjson_is_obj(root) ? root : nullptr;
}

/// One member set on an object, replacing any member of the same name.
void put(yyjson_mut_doc* doc, yyjson_mut_val* object, yyjson_val* key, yyjson_val* value) {
    yyjson_mut_obj_put(object, yyjson_val_mut_copy(doc, key), yyjson_val_mut_copy(doc, value));
}

std::string written(yyjson_mut_doc* doc) {
    std::size_t length = 0;
    char* text = yyjson_mut_write(doc, 0, &length);
    if (text == nullptr)
        throw std::runtime_error("A convention row could not be written as JSON.");
    std::string out(text, length);
    std::free(text);
    return out;
}

/**
 * The columns of a part that the central row owns: its identity, its link to
 * the centre, and its audit provenance. A zip takes only the terms, so the row
 * keeps the version its page was read at.
 */
bool is_link(yyjson_val* key) {
    static const std::set<std::string, std::less<>> links{"id",
                                                          "version",
                                                          "tenant_id",
                                                          "convention_id",
                                                          "party_id",
                                                          "modified_by",
                                                          "performed_by",
                                                          "change_reason_code",
                                                          "change_commentary",
                                                          "recorded_at"};
    return links.contains(std::string_view(yyjson_get_str(key), yyjson_get_len(key)));
}

}

std::vector<convention_screen_routes::family> convention_screen_routes::families() {
    std::vector<family> out;
    for (const auto& u : units)
        out.push_back({std::string(u.key),
                       std::string(u.entity),
                       std::string(u.kind_code),
                       u.write != nullptr,
                       shared_of(u)});
    std::ranges::sort(out, {}, &family::key);
    return out;
}

std::string convention_screen_routes::flatten_central(std::string_view row) {
    const auto in = read_json(row);
    mut_doc_ptr out(yyjson_mut_doc_new(nullptr), &yyjson_mut_doc_free);
    auto* flat = yyjson_mut_obj(out.get());
    yyjson_mut_doc_set_root(out.get(), flat);
    if (auto* root = root_object(in)) {
        std::size_t i = 0, n = 0;
        yyjson_val *key = nullptr, *value = nullptr;
        yyjson_obj_foreach(root, i, n, key, value) {
            if (!yyjson_is_obj(value)) {
                put(out.get(), flat, key, value);
                continue;
            }
            std::size_t j = 0, m = 0;
            yyjson_val *member = nullptr, *member_value = nullptr;
            yyjson_obj_foreach(value, j, m, member, member_value) {
                put(out.get(), flat, member, member_value);
            }
        }
    }
    return written(out.get());
}

std::string convention_screen_routes::zip_parts(std::string_view central, std::string_view item) {
    const auto central_doc = read_json(central);
    const auto item_doc = read_json(item);
    mut_doc_ptr out(yyjson_mut_doc_new(nullptr), &yyjson_mut_doc_free);
    auto* merged = yyjson_mut_obj(out.get());
    yyjson_mut_doc_set_root(out.get(), merged);
    if (auto* root = root_object(central_doc)) {
        std::size_t i = 0, n = 0;
        yyjson_val *key = nullptr, *value = nullptr;
        yyjson_obj_foreach(root, i, n, key, value) {
            put(out.get(), merged, key, value);
        }
    }
    auto* parts = root_object(item_doc);
    if (parts == nullptr)
        return written(out.get());

    std::size_t i = 0, n = 0;
    yyjson_val *name = nullptr, *rows = nullptr;
    yyjson_obj_foreach(parts, i, n, name, rows) {
        if (!yyjson_is_arr(rows))
            continue;
        /*
         * A part array holds one row for each central row it is part of, and
         * none for a form the row does not hold, so the first row is the
         * part's. A term two parts both name takes the later part's value.
         * The BFF zipped the same way.
         */
        yyjson_val* part = nullptr;
        std::size_t r = 0, rn = 0;
        yyjson_val* candidate = nullptr;
        yyjson_arr_foreach(rows, r, rn, candidate) {
            if (yyjson_is_obj(candidate)) {
                part = candidate;
                break;
            }
        }
        if (part == nullptr)
            continue;
        std::size_t j = 0, m = 0;
        yyjson_val *field = nullptr, *field_value = nullptr;
        yyjson_obj_foreach(part, j, m, field, field_value) {
            if (is_link(field))
                continue;
            if (!yyjson_is_obj(field_value)) {
                put(out.get(), merged, field, field_value);
                continue;
            }
            std::size_t k = 0, mk = 0;
            yyjson_val *member = nullptr, *member_value = nullptr;
            yyjson_obj_foreach(field_value, k, mk, member, member_value) {
                if (!is_link(member))
                    put(out.get(), merged, member, member_value);
            }
        }
    }
    return written(out.get());
}

void convention_screen_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using ores::http::forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"conventions"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/conventions/families")
            .summary("List every convention family with its row count and asset class")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::list_convention_kinds_request>([&] {
                    // A kind read that fails leaves every family without an
                    // asset class, and the screen groups them under Other.
                    std::map<std::string, std::string, std::less<>> asset_class_of;
                    try {
                        for (const auto& kind : ores::http::forwarding::forward_all(
                                 req,
                                 session,
                                 messaging::list_convention_kinds_request{},
                                 &messaging::list_convention_kinds_response::kinds))
                            asset_class_of[kind.code] = kind.asset_class;
                    } catch (const rr::refusal& r) {
                        BOOST_LOG_SEV(lg(), ores::logging::warn)
                            << "The convention kinds could not be read: " << r.message;
                    } catch (const std::exception& e) {
                        BOOST_LOG_SEV(lg(), ores::logging::warn)
                            << "The convention kinds could not be read: " << e.what();
                    }

                    std::string out = "{\"families\":[";
                    bool first = true;
                    for (const auto& family : families()) {
                        const auto* u = unit_of(family.key);
                        const auto count = count_of(req, session, *u);
                        const auto asset_class = asset_class_of.find(kind_code_of(family.key));
                        out += first ? "{" : ",{";
                        first = false;
                        out += "\"key\":" + json::write(family.key);
                        out += ",\"entity\":" + json::write(family.entity);
                        out += std::string(",\"writable\":") + (family.writable ? "true" : "false");
                        out += std::string(",\"shared\":") + (family.shared ? "true" : "false");
                        out += ",\"assetClass\":" +
                               (asset_class == asset_class_of.end() || asset_class->second.empty() ?
                                    std::string("null") :
                                    json::write(asset_class->second));
                        out +=
                            ",\"count\":" + (count ? std::to_string(*count) : std::string("null")) +
                            "}";
                    }
                    return http_response::json(out + "]}");
                });
            }));

    add(router->get("/api/conventions/{family}")
            .summary("Read one page of a convention family's rows")
            .query_param("offset", "integer", "", false, "Rows to skip", "0")
            .query_param("limit", "integer", "", false, "Rows to read, 1 to 500", "500")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::list_conventions_request>([&] {
                    const auto& family = family_of(req);
                    const auto offset =
                        bounded(req, "offset", 0, std::numeric_limits<std::uint32_t>::max(), 0);
                    const auto limit = bounded(
                        req, "limit", 1, convention_screen_most_rows, convention_screen_most_rows);
                    const auto page = family.page(req, session, family, offset, limit);
                    return http_response::json("{\"rows\":" + page.rows +
                                               ",\"total\":" + std::to_string(page.total) + "}");
                });
            }));

    add(router->put("/api/conventions/{family}")
            .summary("Write one convention of a family whose terms the screen draws")
            .description("Answers 200 with the service's result whatever its outcome, so read "
                         "result.outcome rather than the status. A family whose terms the screen "
                         "does not draw yet is refused with 400.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::put_deposit_convention_request>([&] {
                    const auto& family = family_of(req);
                    if (family.write == nullptr)
                        rr::invalid("The terms of " + std::string(family.key) +
                                    " conventions are not drawn yet.");
                    return http_response::json(family.write(req, session, req.body));
                });
            }));
}

}
