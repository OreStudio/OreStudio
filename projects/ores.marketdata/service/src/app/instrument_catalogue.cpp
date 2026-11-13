/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#include "ores.marketdata.service/app/instrument_catalogue.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include "ores.marketdata.core/service/series_shape.hpp"
#include "ores.marketdata.service/app/instrument_naming.hpp"
#include "ores.marketdata.service/app/staleness_policy.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <map>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

namespace mm = ores::marketdata::messaging;
namespace dm = ores::marketdata::domain;
namespace dt = ores::marketdata::datum;
using namespace ores::logging;

auto& instrument_catalogue_lg() {
    static auto instance = make_logger("ores.marketdata.service.instrument_catalogue");
    return instance;
}

/// The page a request that states no limit gets.
constexpr std::int64_t default_page = 100;

std::string lower(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return s;
}

struct instrument_draft final {
    std::string instrument_id;
    std::string identity_text;
    std::string party_id;
    std::string kind;
    std::string name;
    std::string currency;
    std::string base_currency;
    std::string quote_currency;
    std::string instrument_type;
    std::string quote_type;
    std::string forward_start;
    std::vector<mm::instrument_axis> axes;
    std::vector<mm::instrument_source> sources;
    std::chrono::system_clock::time_point as_of{};
    bool stale = true;
};

/// Whether the instrument matches the request's free text, whichever of its
/// facts the text names.
bool matches_query(const instrument_draft& i, const std::string& needle) {
    std::string haystack = i.name + " " + i.currency + " " + i.base_currency + " " +
                           i.quote_currency + " " + i.instrument_type;
    for (const auto& s : i.sources)
        haystack += " " + s.provider_code;
    return lower(haystack).find(needle) != std::string::npos;
}

mm::market_instrument to_wire(const instrument_draft& i) {
    mm::market_instrument out;
    out.instrument_id = i.instrument_id;
    out.kind = i.kind;
    out.name = i.name;
    out.currency = i.currency;
    out.base_currency = i.base_currency;
    out.quote_currency = i.quote_currency;
    out.instrument_type = i.instrument_type;
    out.quote_type = i.quote_type;
    out.forward_start = i.forward_start;
    out.axes = i.axes;
    out.sources = i.sources;
    out.party_id = i.party_id;
    out.stale = i.stale;
    out.as_of = i.as_of;
    return out;
}

int kind_rank(const std::string& kind) {
    if (kind == "pair")
        return 0;
    if (kind == "ladder")
        return 1;
    if (kind == "curve")
        return 2;
    if (kind == "surface")
        return 3;
    return 4;
}

}

mm::list_instruments_response list_instruments(const ores::database::context& ctx,
                                               const mm::list_instruments_request& request) {
    mm::list_instruments_response resp;
    const auto now = std::chrono::system_clock::now();

    repository::market_series_repository series_repo;
    repository::market_observation_repository observation_repo;
    repository::provider_repository provider_repo;

    std::map<std::string, std::string> provider_kind;
    for (const auto& p : provider_repo.read_latest(ctx))
        provider_kind[p.code] = p.kind;

    // The parties this session may read: its own, or those it may act for. An
    // empty set is the service's own context, which reads every party.
    std::vector<std::string> allowed;
    if (const auto party = ctx.party_id())
        allowed.push_back(boost::uuids::to_string(*party));
    else
        for (const auto& party : ctx.visible_party_ids())
            allowed.push_back(boost::uuids::to_string(party));

    std::string only_party;
    if (!request.party_id.empty()) {
        for (const auto& party : allowed)
            if (party == request.party_id)
                only_party = party;
        if (only_party.empty()) {
            resp.success = false;
            resp.message =
                "This session may not read the instruments of party " + request.party_id + ".";
            return resp;
        }
    }

    const auto may_read = [&](const std::string& party) {
        if (!only_party.empty())
            return party == only_party;
        return allowed.empty() || std::find(allowed.begin(), allowed.end(), party) != allowed.end();
    };

    std::vector<instrument_draft> instruments;
    std::map<std::string, std::size_t> by_identity;
    std::size_t skipped = 0;

    for (const auto& series : series_repo.read_latest(ctx)) {
        auto parsed = dt::oresmd_uri_codec::read(series.oresmd_uri);
        if (!parsed || !parsed->is_series()) {
            // A fixing is an index, not a datum: its URI is read by read_index
            // and its values live in market_fixings. The catalogue does not
            // classify them yet, so it says so below rather than inventing a
            // row for one.
            ++skipped;
            BOOST_LOG_SEV(instrument_catalogue_lg(), debug)
                << "Not catalogued, not a datum series: " << series.oresmd_uri;
            continue;
        }
        const auto& row = dt::schema_of(parsed->type());
        const auto party_id = boost::uuids::to_string(series.party_id);
        if (!may_read(party_id))
            continue;

        const auto series_id = boost::uuids::to_string(series.id);
        const auto identity = party_id + "|" + series.oresmd_uri;

        const auto shape = read_series_shape(ctx, series_id);
        const auto kind = kind_of(row, shape.axes.size());
        if (kind.empty())
            continue;

        auto found = by_identity.find(identity);
        if (found == by_identity.end()) {
            instrument_draft draft;
            draft.instrument_id = identity;
            draft.identity_text = series.oresmd_uri;
            draft.party_id = party_id;
            draft.kind = kind;
            draft.instrument_type =
                std::string(dt::oresmd_uri_codec::instrument_spelling(parsed->type()));
            draft.quote_type = std::string(dt::oresmd_uri_codec::quote_spelling(parsed->quote()));
            draft.name = name_of(*parsed, row, kind, draft.instrument_type);
            draft.currency = field_text(*parsed, dt::field::ccy);
            draft.base_currency = field_text(*parsed, dt::field::unit_ccy);
            draft.quote_currency =
                draft.base_currency.empty() ? std::string() : field_text(*parsed, dt::field::ccy);
            draft.forward_start = field_text(*parsed, dt::field::fwd_start);
            for (const auto axis : shape.axes) {
                mm::instrument_axis a;
                a.field = std::string(dt::name_of(axis));
                auto values = shape.values.find(axis);
                if (values != shape.values.end())
                    a.values = values->second;
                draft.axes.push_back(std::move(a));
            }
            found = by_identity.emplace(identity, instruments.size()).first;
            instruments.push_back(std::move(draft));
        }
        auto& instrument = instruments[found->second];

        // The source's own instant: the newest observation the series holds.
        std::optional<std::chrono::system_clock::time_point> newest;
        for (const auto& o : observation_repo.read_latest_for_series(ctx, series.id))
            if (!newest || o.observation_datetime > *newest)
                newest = o.observation_datetime;

        mm::instrument_source source;
        source.provider_code = series.provider_code;
        const auto pk = provider_kind.find(series.provider_code);
        if (pk != provider_kind.end())
            source.provider_kind = pk->second;
        source.series_id = series_id;
        source.as_of = newest.value_or(std::chrono::system_clock::time_point{});
        source.age_seconds = staleness_policy::age_seconds(source.as_of, now);
        source.stale = staleness_policy::is_stale(source.as_of, now);
        instrument.sources.push_back(std::move(source));

        if (!newest || newest > instrument.as_of)
            instrument.as_of = newest.value_or(instrument.as_of);
    }

    // An instrument is stale when every source is, or when it has none.
    for (auto& instrument : instruments)
        instrument.stale = std::all_of(instrument.sources.begin(),
                                       instrument.sources.end(),
                                       [](const auto& s) { return s.stale; });

    const auto needle = lower(request.query);
    std::vector<const instrument_draft*> selected;
    for (const auto& i : instruments) {
        if (!request.kinds.empty() &&
            std::find(request.kinds.begin(), request.kinds.end(), i.kind) == request.kinds.end())
            continue;
        if (!request.provider_code.empty() &&
            std::none_of(i.sources.begin(), i.sources.end(), [&](const auto& s) {
                return s.provider_code == request.provider_code;
            }))
            continue;
        if (!needle.empty() && !matches_query(i, needle))
            continue;
        selected.push_back(&i);
    }

    std::sort(selected.begin(), selected.end(), [](const auto* a, const auto* b) {
        if (a->kind != b->kind)
            return kind_rank(a->kind) < kind_rank(b->kind);
        return a->name < b->name;
    });

    resp.total = static_cast<std::int64_t>(selected.size());
    const auto limit = request.limit > 0 ? request.limit : default_page;
    const auto offset = request.offset > 0 ? request.offset : 0;
    for (std::int64_t i = offset;
         i < static_cast<std::int64_t>(selected.size()) && i < offset + limit;
         ++i)
        resp.instruments.push_back(to_wire(*selected[static_cast<std::size_t>(i)]));

    resp.truncated = offset + static_cast<std::int64_t>(resp.instruments.size()) < resp.total;

    const bool fixing_only =
        !request.kinds.empty() && std::all_of(request.kinds.begin(),
                                              request.kinds.end(),
                                              [](const auto& k) { return k == "fixing"; });
    if (fixing_only && skipped > 0) {
        resp.success = false;
        resp.message = "Fixing instruments are not catalogued yet: a fixing is an "
                       "index whose values are read from the fixings table, not "
                       "from observations.";
        return resp;
    }

    resp.success = true;
    return resp;
}

}
