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
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.marketdata.api/domain/market_fixing.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.core/market/fixing.hpp"
#include "ores.ore.core/market/fx_quote_convention_checker.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <algorithm>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>

namespace ores::marketdata::service {

namespace {

using ores::marketdata::core::oresmd_parser;
using ores::marketdata::core::oresmd_projections;

auto& import_helpers_lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.marketdata.service.import_service");
    return instance;
}

// Fetches every known currency pair from ores.refdata (paginated), for
// fx_quote_convention_checker. Best-effort: any failure (refdata
// unreachable, decode error, permission denied) logs a warning and returns
// an empty set, so an import never fails just because this side-check
// couldn't run — the reversed-key correction is then simply skipped.
std::set<ores::ore::market::fx_quote_convention_checker::currency_pair>
fetch_known_currency_pairs(ores::nats::service::nats_client& auth_nats) {
    using namespace ores::logging;
    std::set<ores::ore::market::fx_quote_convention_checker::currency_pair> pairs;
    try {
        std::uint32_t offset = 0;
        constexpr std::uint32_t page_size = 200;
        for (;;) {
            ores::refdata::messaging::get_currency_pairs_request req;
            req.offset = offset;
            req.limit = page_size;
            const auto& codec = ores::nats::default_wire_codec();
            const auto reply = auth_nats.authenticated_request(req.nats_subject, codec.encode(req));
            auto resp =
                codec.decode<ores::refdata::messaging::get_currency_pairs_response>(reply.data);
            if (!resp || !resp->success) {
                BOOST_LOG_SEV(import_helpers_lg(), warn)
                    << "Failed to fetch currency pairs for FX quote convention checking; "
                    << "reversed-key correction disabled for this import.";
                return {};
            }
            for (const auto& p : resp->pairs)
                pairs.emplace(p.base_currency, p.quote_currency);
            offset += static_cast<std::uint32_t>(resp->pairs.size());
            if (resp->pairs.empty() ||
                offset >= static_cast<std::uint32_t>(resp->total_available_count))
                break;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(import_helpers_lg(), warn)
            << "Failed to fetch currency pairs (" << e.what()
            << "); reversed-key correction disabled for this import.";
        return {};
    }
    return pairs;
}

} // namespace

import_service::import_service(context ctx, ores::nats::service::nats_client& auth_nats)
    : ctx_(std::move(ctx))
    , auth_nats_(auth_nats) {}

messaging::import_market_data_response
import_service::import(const messaging::import_market_data_request& req) {
    messaging::import_market_data_response resp;
    boost::uuids::random_generator gen;
    repository::market_series_repository series_repo;
    repository::market_observations_repository obs_repo;
    repository::market_fixings_repository fixings_repo;

    // Cache: canonical oresmd_uri → (series.id, is_scalar). The scalar flag
    // is derived from the identifier at the edge (is_scalar()); the series row
    // stores no classification columns.
    struct series_info {
        boost::uuids::uuid id;
        bool is_scalar;
    };
    std::map<std::string, series_info> series_cache;

    auto find_or_create_series = [&](const std::string& oresmd_uri, bool is_scalar) -> series_info {
        const auto it = series_cache.find(oresmd_uri);
        if (it != series_cache.end())
            return it->second;

        // Look up existing series in DB.
        const auto existing = series_repo.read_latest_by_uri(ctx_, oresmd_uri);
        if (!existing.empty()) {
            const series_info info{existing.front().id, is_scalar};
            series_cache.emplace(oresmd_uri, info);
            return info;
        }

        // Create a new series.
        domain::market_series s;
        s.id = gen();
        s.tenant_id = ctx_.tenant_id();
        s.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
        s.oresmd_uri = oresmd_uri;
        s.modified_by = ctx_.actor();
        s.performed_by = ctx_.service_account();
        s.change_reason_code =
            std::string(dq::domain::change_reason_constants::codes::external_data_import);
        s.change_commentary = "Imported from ORE market data file";
        series_repo.write(ctx_, s);

        const series_info info{s.id, is_scalar};
        series_cache.emplace(oresmd_uri, info);
        ++resp.series_count;
        return info;
    };

    const auto on_duplicate = req.duplicates_are_errors ?
                                  ores::ore::market::duplicate_policy::error :
                                  ores::ore::market::duplicate_policy::warn;

    auto append_issues = [](std::vector<std::string>& dest,
                            const std::vector<ores::ore::market::parse_issue>& issues,
                            const std::string& section) {
        for (const auto& issue : issues)
            dest.push_back(section + " line " + std::to_string(issue.line_no) + ": " +
                           issue.message);
    };

    // ── Import market observations ────────────────────────────────────────────
    if (!req.market_data_content.empty()) {
        std::istringstream in(req.market_data_content);
        ores::ore::market::parse_report report;
        auto data = ores::ore::market::parse_market_data(in, on_duplicate, &report);
        append_issues(resp.warnings, report.warnings, "market data");
        append_issues(resp.errors, report.errors, "market data");

        // Best-effort FX/RATE convention correction: a reversed key (e.g.
        // some vendored ORE example market.txt files store GBP/USD under
        // FX/RATE/USD/GBP — see fx_quote_convention_checker's docs) is
        // detected against ores.refdata's currency_pair reference data and
        // swapped in the identifier's pair at conversion time, before
        // persistence, so every downstream consumer (including the series
        // this creates) sees the canonical URI. The value is never touched
        // — only the pair is swapped — so this can never introduce
        // floating-point error. The checker is empty (a no-op) when refdata
        // is unreachable or the import has no FX/RATE rows.
        std::set<ores::ore::market::fx_quote_convention_checker::currency_pair> known_pairs;
        const auto has_fx_rate = std::any_of(data.begin(), data.end(), [](const auto& d) {
            return d.series_type == "FX" && d.metric == "RATE";
        });
        if (has_fx_rate)
            known_pairs = fetch_known_currency_pairs(auth_nats_);
        const ores::ore::market::fx_quote_convention_checker checker(known_pairs);

        // parse_market_data already de-duplicated repeated (date, key)
        // pairs (last-line-wins) — see duplicate_policy. In error mode,
        // skip persisting this content rather than silently importing
        // data the caller asked to be told about instead.
        if (report.errors.empty()) {
            std::vector<domain::market_observation> observations;
            observations.reserve(data.size());
            for (const auto& d : data) {
                // The full ORE key, point included: the inverse projection
                // maps complete keys only (IR_SWAP with its six segments,
                // SWAPTION with its multi-segment point, ...).
                const auto key = d.series_type + "/" + d.metric + "/" + d.qualifier +
                                 (d.point_id ? "/" + *d.point_id : "");
                auto id = oresmd_projections::from_ore_key(key, checker);
                if (!id) {
                    // No oresmd mapping (BOND, the option/capfloor families,
                    // malformed keys): drop the row, visibly.
                    resp.warnings.push_back("Skipped ORE key '" + key +
                                            "': no oresmd URI mapping.");
                    continue;
                }
                if (d.series_type == "FX" && d.metric == "RATE") {
                    const auto* fx = std::get_if<domain::fx_market_data_identifier>(&*id);
                    if (fx) {
                        const auto raw = oresmd_projections::from_ore_key(key);
                        if (raw) {
                            const auto* raw_fx =
                                std::get_if<domain::fx_market_data_identifier>(&*raw);
                            if (raw_fx && raw_fx->pair != fx->pair)
                                resp.warnings.push_back(
                                    "FX/RATE/" + raw_fx->pair.substr(0, 3) + "/" +
                                    raw_fx->pair.substr(3, 3) + " = " + d.value + " -> FX/RATE/" +
                                    fx->pair.substr(0, 3) + "/" + fx->pair.substr(3, 3) + " = " +
                                    d.value +
                                    " (value unchanged): reversed relative to refdata's canonical "
                                    "currency pair.");
                        }
                    }
                }
                const auto series = find_or_create_series(
                    oresmd_parser::to_uri(*id).value, oresmd_projections::is_scalar(*id));

                domain::market_observation obs;
                obs.id = gen();
                obs.tenant_id = ctx_.tenant_id();
                obs.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
                obs.series_id = series.id;
                obs.observation_datetime = std::chrono::sys_days{d.date};
                // Scalar series (FX spot, equity spot, ...) have no tenor/surface
                // coordinate; point_id defaults to "SPOT" for those. Non-scalar
                // series with a missing point_id (a URI whose identifier carries
                // no point) keep the empty default rather than being mislabelled
                // as spot.
                obs.point_id = d.point_id.value_or(series.is_scalar ? "SPOT" : "");
                obs.source = req.source;
                obs.value = d.value;
                observations.push_back(std::move(obs));
            }

            obs_repo.write(ctx_, observations);
            resp.observation_count = static_cast<int>(observations.size());
        }
    }

    // ── Import fixings ────────────────────────────────────────────────────────
    if (!req.fixings_content.empty()) {
        std::istringstream in(req.fixings_content);
        ores::ore::market::parse_report report;
        const auto data = ores::ore::market::parse_fixings(in, on_duplicate, &report);
        append_issues(resp.warnings, report.warnings, "fixings");
        append_issues(resp.errors, report.errors, "fixings");

        if (report.errors.empty()) {
            std::vector<domain::market_fixing> fixings;
            fixings.reserve(data.size());
            for (const auto& f : data) {
                // Fixing series: the index name (e.g. "USD-LIBOR-3M") is the
                // fixing boundary's key space, converted via from_index_name —
                // the registry-backed from_ore_key() does not seed it.
                const auto id = oresmd_projections::from_index_name(f.qualifier);
                if (!id) {
                    resp.warnings.push_back("Skipped fixing '" + f.qualifier +
                                            "': no oresmd URI mapping.");
                    continue;
                }
                const auto series = find_or_create_series(oresmd_parser::to_uri(*id).value,
                                                          oresmd_projections::is_scalar(*id));

                domain::market_fixing fix;
                fix.id = gen();
                fix.tenant_id = ctx_.tenant_id();
                fix.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
                fix.series_id = series.id;
                fix.fixing_date = f.date;
                fix.source = req.source;
                fix.value = f.value;
                fixings.push_back(std::move(fix));
            }

            fixings_repo.write(ctx_, fixings);
            resp.fixing_count = static_cast<int>(fixings.size());
        }
    }

    resp.success = resp.errors.empty();
    if (!resp.success) {
        resp.message = std::to_string(resp.errors.size()) +
                       " duplicate error(s) found; affected content was not imported.";
    }
    return resp;
}

}
