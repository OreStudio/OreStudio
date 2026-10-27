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
#include "ores.ore.service/messaging/currency_config_handler.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.ore.api/messaging/currency_config_protocol.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.ore.service/messaging/nats_call.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "shared_helpers.hpp"
#include <algorithm>
#include <cctype>
#include <cstdint>
#include <format>
#include <iterator>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::ore::messaging;
using ores::refdata::domain::currency;

namespace {

constexpr std::string_view document_name = "currencyconfig.xml";
constexpr std::string_view root_element = "CurrencyConfig";
// The name of the first element of a document, past the prolog, a comment or a
// document type declaration. The real parse follows, so this only has to be
// right enough to name the kind of file in a refusal.
std::string root_element_of(std::string_view text) {
    std::size_t at = text.find('<');
    while (at != std::string_view::npos) {
        if (text.substr(at, 4) == "<!--") {
            const auto end = text.find("-->", at + 4);
            if (end == std::string_view::npos)
                return {};
            at = text.find('<', end + 3);
            continue;
        }
        if (at + 1 < text.size() && (text[at + 1] == '?' || text[at + 1] == '!')) {
            std::size_t end = at + 2;
            int brackets = 0;
            for (; end < text.size(); ++end) {
                if (text[end] == '[')
                    ++brackets;
                else if (text[end] == ']')
                    --brackets;
                else if (text[end] == '>' && brackets <= 0)
                    break;
            }
            if (end >= text.size())
                return {};
            at = text.find('<', end + 1);
            continue;
        }
        auto end = at + 1;
        while (end < text.size() &&
               (std::isalnum(static_cast<unsigned char>(text[end])) || text[end] == '_' ||
                text[end] == ':' || text[end] == '-' || text[end] == '.'))
            ++end;
        return std::string(text.substr(at + 1, end - at - 1));
    }
    return {};
}

// Sends a request and returns its response, or throws with the owner's reason.
template <typename Req>
typename Req::response_type call(ores::nats::service::nats_client& owners, const Req& req) {
    std::string error;
    auto resp = nats_call(owners, req, error);
    if (!resp || !error.empty())
        throw std::runtime_error(error.empty() ? "no response" : error);
    return *resp;
}

// Every currency the caller may read, a page at a time.
std::vector<currency> read_all_currencies(ores::nats::service::nats_client& owners) {
    std::vector<currency> all;
    for (std::uint32_t offset = 0;; offset += page) {
        ores::refdata::messaging::list_currencies_request req;
        req.offset = offset;
        req.limit = page;
        auto resp = call(owners, req);
        const auto got = resp.currencies.size();
        all.insert(all.end(),
                   std::make_move_iterator(resp.currencies.begin()),
                   std::make_move_iterator(resp.currencies.end()));
        if (got < page)
            return all;
    }
}

std::vector<currency> parse_document(const std::string& content) {
    const auto root = root_element_of(content);
    if (root != root_element)
        throw std::runtime_error(std::format(
            "This file is not a currency configuration: its root element is {}. Choose a {} file.",
            root.empty() ? std::string("missing") : root,
            root_element));
    return ores::ore::xml::importer::import_currency_config_text(content);
}

ores::refdata::messaging::currency_write to_write(const currency& c) {
    return {.iso_code = c.iso_code,
            .name = c.name,
            .numeric_code = c.numeric_code,
            .symbol = c.symbol,
            .fraction_symbol = c.fraction_symbol,
            .fractions_per_unit = c.fractions_per_unit,
            .rounding_type = c.rounding_type,
            .rounding_precision = c.rounding_precision,
            .format = c.format,
            .monetary_nature = c.monetary_nature,
            .market_tier = c.market_tier,
            .ore_currency_type = c.ore_currency_type,
            .image_id = c.image_id,
            .spot_days = c.spot_days,
            .day_basis = c.day_basis,
            .base_precedence = c.base_precedence};
}

// The document carries a currency's name, codes, symbols, rounding and ORE type
// and nothing else: the format, the market tier, the spot days, the day basis,
// the base precedence and the flag are the tenant's, so a difference is read on
// the document's fields alone, and a write keeps the tenant's values for the rest.
constexpr std::string_view fields_not_in_document =
    "Format, Monetary nature, Market tier, Spot days, Day basis, Base precedence, Flag";

std::string what_differs(const currency& ours, const currency& theirs) {
    std::vector<std::string> parts;
    const auto note = [&](std::string_view label, const auto& a, const auto& b) {
        if (a != b)
            parts.push_back(std::format("{} {} → {}", label, a, b));
    };
    const auto text = [](const std::optional<std::string>& v) {
        return v.value_or("—");
    };
    note("Name", ours.name, theirs.name);
    note("Numeric code", ours.numeric_code, theirs.numeric_code);
    note("Symbol", ours.symbol, theirs.symbol);
    note("Fraction symbol", ours.fraction_symbol, theirs.fraction_symbol);
    note("Fractions per unit", ours.fractions_per_unit, theirs.fractions_per_unit);
    note("Rounding type", ours.rounding_type, theirs.rounding_type);
    note("Rounding precision", ours.rounding_precision, theirs.rounding_precision);
    note("ORE currency type", text(ours.ore_currency_type), text(theirs.ore_currency_type));
    std::string out;
    for (const auto& p : parts)
        out += (out.empty() ? "" : "; ") + p;
    return out;
}

// The tenant's currency with the document's fields laid over it.
currency merged(const currency& ours, const currency& theirs) {
    currency c = ours;
    c.name = theirs.name;
    c.numeric_code = theirs.numeric_code;
    c.symbol = theirs.symbol;
    c.fraction_symbol = theirs.fraction_symbol;
    c.fractions_per_unit = theirs.fractions_per_unit;
    c.rounding_type = theirs.rounding_type;
    c.rounding_precision = theirs.rounding_precision;
    c.ore_currency_type = theirs.ore_currency_type;
    return c;
}

struct difference {
    std::map<std::string, currency> ours;
    std::vector<currency> theirs;
    std::vector<currency_import_row> rows;
    std::vector<currency_import_issue> issues;
    std::uint32_t held = 0;
};

// Reads what the tenant holds, parses the file, and says what the import would do.
difference compute_difference(ores::nats::service::nats_client& owners,
                              const std::string& content) {
    difference d;
    for (auto& c : read_all_currencies(owners))
        d.ours.emplace(c.iso_code, std::move(c));

    d.issues.push_back({.kind = "dropped",
                        .subject = std::string(fields_not_in_document),
                        .detail = "The document has no element for these. A currency the tenant "
                                  "holds keeps its values; a new one starts without them."});
    std::set<std::string> seen;
    for (auto& c : parse_document(content)) {
        if (!seen.insert(c.iso_code).second) {
            d.issues.push_back(
                {.kind = "duplicate",
                 .subject = c.iso_code,
                 .detail = "The file names this currency twice; the first is kept."});
            continue;
        }
        if (const auto problem = ores::ore::xml::importer::validate_currency(c); !problem.empty())
            d.issues.push_back({.kind = "value", .subject = c.iso_code, .detail = problem});
        d.theirs.push_back(std::move(c));
    }

    for (const auto& c : d.theirs) {
        const auto it = d.ours.find(c.iso_code);
        if (it == d.ours.end()) {
            d.rows.push_back({.iso_code = c.iso_code,
                              .name = c.name,
                              .change = "added",
                              .what = "New to the tenant"});
            continue;
        }
        const auto what = what_differs(it->second, c);
        if (what.empty())
            ++d.held;
        else
            d.rows.push_back(
                {.iso_code = c.iso_code, .name = c.name, .change = "changed", .what = what});
    }
    for (const auto& [code, c] : d.ours)
        if (!seen.contains(code))
            d.rows.push_back(
                {.iso_code = code, .name = c.name, .change = "removed", .what = "Not in the file"});
    return d;
}

// The choice the person made for a row, or the default: a row the file omits
// is kept, a new or changed one takes the file's. A choice a row cannot take,
// such as deleting a currency the file still holds, is refused as an error.
std::optional<std::string> action_for(const import_currency_config_request& req,
                                      const currency_import_row& row) {
    std::string action = row.change == "removed" ? "keep" : "take";
    for (const auto& choice : req.choices)
        if (choice.iso_code == row.iso_code)
            action = choice.action;
    const bool fits = row.change == "removed" ? (action == "keep" || action == "delete") :
                      row.change == "added"   ? (action == "take" || action == "skip") :
                                                (action == "take" || action == "keep");
    if (!fits)
        return std::nullopt;
    return action;
}

}

currency_config_handler::currency_config_handler(ores::nats::service::client& nats,
                                                 ores::database::context ctx,
                                                 ores::security::jwt::jwt_authenticator signer,
                                                 ores::nats::service::nats_client outbound_nats)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , outbound_nats_(std::move(outbound_nats)) {}

void currency_config_handler::export_currency_config(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    if (!has_permission(*ctx, "refdata::currencies:read")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<export_currency_config_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        auto owners = outbound_nats_.with_delegation(ores::nats::service::extract_bearer(msg))
                          .with_correlation_id(correlation_id);
        auto currencies = read_all_currencies(owners);
        if (!req->iso_codes.empty()) {
            const std::set<std::string> wanted(req->iso_codes.begin(), req->iso_codes.end());
            std::erase_if(currencies, [&](const auto& c) { return !wanted.contains(c.iso_code); });
        }
        export_currency_config_response response;
        response.count = static_cast<std::uint32_t>(currencies.size());
        response.files.push_back(
            {.name = std::string(document_name),
             .content = ores::ore::xml::exporter::export_currency_config(currencies)});
        response.success = true;
        response.message = std::format("Exported {} currencies.", currencies.size());
        BOOST_LOG_SEV(lg(), info) << "currency config exported | corr=" << correlation_id
                                  << " count=" << currencies.size();
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "currency config export refused | corr=" << correlation_id
                                  << " " << e.what();
        reply(nats_, msg, export_currency_config_response{.message = e.what()});
    }
}

void currency_config_handler::preview_currency_config_import(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    if (!has_permission(*ctx, "refdata::currencies:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<preview_currency_config_import_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    preview_currency_config_import_response response;
    response.root_element = root_element_of(req->content);
    try {
        auto owners = outbound_nats_.with_delegation(ores::nats::service::extract_bearer(msg))
                          .with_correlation_id(correlation_id);
        auto d = compute_difference(owners, req->content);
        response.in_file = static_cast<std::uint32_t>(d.theirs.size());
        response.held = d.held;
        for (const auto& row : d.rows) {
            if (row.change == "added")
                ++response.added;
            else if (row.change == "changed")
                ++response.changed;
            else
                ++response.removed;
        }
        response.rows = std::move(d.rows);
        response.issues = std::move(d.issues);
        response.success = true;
        response.message = std::format("{} currencies in the file.", response.in_file);
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "currency config preview refused | corr=" << correlation_id
                                  << " " << e.what();
        response.message = e.what();
        reply(nats_, msg, response);
    }
}

void currency_config_handler::import_currency_config(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    if (!has_permission(*ctx, "refdata::currencies:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<import_currency_config_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    import_currency_config_response response;
    try {
        auto owners = outbound_nats_.with_delegation(ores::nats::service::extract_bearer(msg))
                          .with_correlation_id(correlation_id);
        const auto d = compute_difference(owners, req->content);
        const ores::utility::domain::change_intent intent{
            .reason_code = req->reason_code.empty() ? "system.import" : req->reason_code,
            .commentary = req->commentary.empty() ? "Imported from an ORE currency configuration" :
                                                    req->commentary};
        for (const auto& row : d.rows) {
            const auto action = action_for(*req, row);
            if (!action) {
                response.errors.push_back(
                    {row.iso_code,
                     std::format("The choice does not fit a {} currency.", row.change)});
                continue;
            }
            if (*action == "keep" || *action == "skip") {
                ++response.kept;
                continue;
            }
            std::string error;
            if (*action == "delete") {
                ores::refdata::messaging::delete_currency_request remove{
                    .removal = {.key = {.iso_code = row.iso_code}}, .intent = intent};
                auto resp = nats_call(owners, remove, error);
                if (!resp || !error.empty())
                    response.errors.push_back(
                        {row.iso_code, error.empty() ? "no response" : error});
                else
                    ++response.deleted;
                continue;
            }
            const auto theirs = std::ranges::find_if(
                d.theirs, [&](const auto& c) { return c.iso_code == row.iso_code; });
            if (theirs == d.theirs.end()) {
                response.errors.push_back({row.iso_code, "The file does not hold this currency."});
                continue;
            }
            const auto ours = d.ours.find(row.iso_code);
            auto write = to_write(ours == d.ours.end() ? *theirs : merged(ours->second, *theirs));
            ores::refdata::messaging::put_currency_request put{
                .change =
                    {.write = std::move(write),
                     .precondition =
                         ours == d.ours.end() ?
                             ores::utility::domain::precondition{} :
                             ores::utility::domain::precondition{
                                 .kind =
                                     ores::utility::domain::precondition_kind::must_match_version,
                                 .version = static_cast<std::uint32_t>(ours->second.version)}},
                .intent = intent};
            auto resp = nats_call(owners, put, error);
            if (!resp || !error.empty()) {
                response.errors.push_back({row.iso_code, error.empty() ? "no response" : error});
                continue;
            }
            if (ours == d.ours.end())
                ++response.added;
            else
                ++response.changed;
        }
        response.success = response.errors.empty();
        response.message = std::format("Imported: {} added, {} changed, {} deleted, {} kept{}.",
                                       response.added,
                                       response.changed,
                                       response.deleted,
                                       response.kept,
                                       response.errors.empty() ?
                                           std::string() :
                                           std::format("; {} not written", response.errors.size()));
        BOOST_LOG_SEV(lg(), info) << "currency config imported | corr=" << correlation_id << " "
                                  << response.message;
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "currency config import refused | corr=" << correlation_id
                                  << " " << e.what();
        response.message = e.what();
        reply(nats_, msg, response);
    }
}

}
