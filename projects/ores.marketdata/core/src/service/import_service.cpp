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
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.core/classification/series_classifier.hpp"
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.core/market/fixing.hpp"
#include "ores.ore.core/market/fx_quote_convention_checker.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.ore.core/market/series_key_registry.hpp"
#include "ores.ore.core/repository/series_key_shape_repository.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <algorithm>
#include <map>
#include <optional>
#include <rfl/enums.hpp>
#include <set>
#include <sstream>
#include <stdexcept>
#include <tuple>
#include <vector>

namespace ores::marketdata::service {

namespace {

auto& import_helpers_lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.marketdata.service.import_service");
    return instance;
}

// One junction row per asset class, carrying the series' own audit trail so
// the two tables read as one change.
domain::market_series_asset_class make_asset_class_row(const domain::market_series& series,
                                                       const std::string& code) {
    domain::market_series_asset_class row;
    row.tenant_id = series.tenant_id.to_string();
    row.market_series_id = series.id;
    row.asset_class_code = code;
    row.modified_by = series.modified_by;
    row.performed_by = series.performed_by;
    row.change_reason_code = series.change_reason_code;
    row.change_commentary = series.change_commentary;
    return row;
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
            ores::refdata::messaging::list_currency_pairs_request req;
            req.offset = offset;
            req.limit = page_size;
            const auto& codec = ores::nats::default_wire_codec();
            const auto reply = auth_nats.authenticated_request(req.nats_subject, codec.encode(req));
            auto resp =
                codec.decode<ores::refdata::messaging::list_currency_pairs_response>(reply.data);
            if (!resp || resp->result.outcome != ores::utility::domain::outcome::ok) {
                BOOST_LOG_SEV(import_helpers_lg(), warn)
                    << "Failed to fetch currency pairs for FX quote convention checking; "
                    << "reversed-key correction disabled for this import.";
                return {};
            }
            for (const auto& p : resp->pairs)
                pairs.emplace(p.base_currency, p.quote_currency);
            offset += static_cast<std::uint32_t>(resp->pairs.size());
            if (resp->pairs.empty() || offset >= static_cast<std::uint32_t>(resp->total))
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

// What oresmd makes of one parsed key: the canonical spelling it projects back
// to, the registry's decomposition of that spelling into the columns a series
// and its observations are stored under, and whether the difference between the
// two is the FX convention checker reversing the pair.
//
// The last one is worth separating. A spelling difference is the grammar
// tidying up after the producer; a reversed pair means the file stored the
// instrument the wrong way round against refdata, and the operator is the only
// one who can fix the file. Reporting both as "not canonical" hides the second.
struct named_key final {
    std::string canonical;
    ores::ore::market::decomposed_key decomposition;
    /// The identity the key projects to, as a URI. This is what the series is meant
    /// to be read by; the canonical spelling beside it is what the file's key
    /// becomes, which a consumer of ORE keys still needs.
    std::string uri;
    bool fx_pair_reversed = false;
};

// Whether the convention checker is what moved this key's currencies, as opposed
// to a spelling the grammar corrected. Only an FX identifier carries a pair, and
// only a reversed pair makes the checker change it.
bool fx_pair_moved(const std::string& key, const domain::market_data_identifier& corrected) {
    const auto* after = std::get_if<domain::fx_market_data_identifier>(&corrected);
    if (!after)
        return false;
    const auto before = core::oresmd_projections::from_ore_key(key);
    if (!before)
        return false;
    const auto* was = std::get_if<domain::fx_market_data_identifier>(&*before);
    return was && was->pair != after->pair;
}

// The oresmd grammar is the authority for what an ORE key means. A key it can
// name is stored under its canonical spelling, so two spellings of one
// instrument reach one series rather than two; a key it cannot name returns
// nullopt, and the caller drops the row rather than filing it under a series
// with no identity. Every key the corpus carries names, so this is the guard
// for a file that carries one the grammar has no class for.
std::optional<named_key>
canonical_key(const std::string& key,
              const ores::ore::market::series_key_registry& registry,
              const ores::ore::market::fx_quote_convention_checker* fx_checker) {
    const auto identifier = fx_checker ? core::oresmd_projections::from_ore_key(key, *fx_checker) :
                                         core::oresmd_projections::from_ore_key(key);
    if (!identifier)
        return std::nullopt;
    auto canonical = core::oresmd_projections::to_quote_key(*identifier);
    if (!canonical)
        return std::nullopt;
    named_key result;
    result.decomposition = registry.decompose(*canonical);
    result.canonical = std::move(*canonical);
    // The file's key names one datum, and the series is the datum's identity with
    // its point dropped: the series is what the row above holds, and its points are
    // the observations beneath it.
    result.uri = core::oresmd_parser::to_series_uri(*identifier).value;
    // Only asked when there is a difference to explain, so the second projection
    // costs nothing on the keys that already read back as they arrived.
    result.fx_pair_reversed =
        fx_checker && result.canonical != key && fx_pair_moved(key, *identifier);
    return result;
}

} // namespace

import_service::import_service(context ctx,
                               ores::nats::service::nats_client& auth_nats,
                               known_pairs_provider known_pairs)
    : ctx_(std::move(ctx))
    , auth_nats_(auth_nats)
    , known_pairs_(std::move(known_pairs)) {}

messaging::import_market_data_response
import_service::import(const messaging::import_market_data_request& req) {
    messaging::import_market_data_response resp;
    boost::uuids::random_generator gen;
    repository::market_series_repository series_repo;
    repository::market_series_asset_class_repository series_asset_class_repo(ctx_);
    repository::market_observations_repository obs_repo;
    repository::market_fixings_repository fixings_repo;

    // Cache: the series' oresmd identity → series id.
    std::map<std::string, boost::uuids::uuid> series_cache;

    // Read the classification rules once for the whole batch, on the first
    // series that needs them. Both the market data and the fixings paths
    // create series, so the read belongs to neither branch; a request that
    // carries neither payload never pays for it.
    std::optional<core::series_classifier> classifier;

    // The identity the series is given: the oresmd URI its key or index name
    // projects to. Every caller has one, because a row whose key the grammar cannot
    // name is not imported at all.
    auto find_or_create_series = [&](const std::string& series_type,
                                     const std::string& metric,
                                     const std::string& qualifier,
                                     const std::string& oresmd_uri) -> boost::uuids::uuid {
        // The identity is what the series is, so the cache is keyed by it rather
        // than by the triple the registry decomposed the same key into.
        const auto key = oresmd_uri;
        const auto it = series_cache.find(key);
        if (it != series_cache.end())
            return it->second;

        // Found by the identity, which is what the row is keyed by. There is no
        // triple fallback: every row carries an identity, and a row an earlier
        // database named synthetically is not the series this key is -- reusing it
        // would keep the synthetic name for good.
        auto existing = series_repo.read_latest_by_uri(ctx_, oresmd_uri);
        if (!existing.empty()) {
            const auto id = existing.front().id;
            series_cache.emplace(key, id);
            return id;
        }

        // Create a new series.
        if (!classifier)
            classifier.emplace(
                repository::series_classification_rule_repository{}.read_latest(ctx_));
        const auto cl = classifier->classify(series_type, metric, qualifier);
        domain::market_series s;
        s.id = gen();
        s.tenant_id = ctx_.tenant_id();
        s.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
        s.series_type = series_type;
        s.metric = metric;
        s.qualifier = qualifier;
        s.series_subclass = cl.series_subclass;
        s.oresmd_uri = oresmd_uri;
        s.modified_by = ctx_.actor();
        s.performed_by = ctx_.service_account();
        s.change_reason_code =
            std::string(dq::domain::change_reason_constants::codes::external_data_import);
        s.change_commentary = "Imported from ORE market data file";
        series_repo.write(ctx_, s);

        std::vector<domain::market_series_asset_class> classes;
        classes.reserve(cl.asset_classes.size());
        for (const auto& code : cl.asset_classes)
            classes.push_back(make_asset_class_row(s, code));
        if (!classes.empty())
            series_asset_class_repo.write(classes);

        series_cache.emplace(key, s.id);
        ++resp.series_count;
        return s.id;
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
        // Read the key grammar once for the whole batch. A fixings-only
        // import never gets here, so it never pays for the read.
        const ores::ore::market::series_key_registry registry{
            ores::ore::repository::series_key_shape_repository{}.read_latest(ctx_)};
        auto data = ores::ore::market::parse_market_data(in, registry, on_duplicate, &report);
        append_issues(resp.warnings, report.warnings, "market data");
        append_issues(resp.errors, report.errors, "market data");

        // Best-effort: a reversed FX/RATE key (e.g. some vendored ORE
        // example market.txt files store GBP/USD under FX/RATE/USD/GBP —
        // see fx_quote_convention_checker's docs) is detected against
        // ores.refdata's currency_pair reference data and corrected before
        // persistence, so every downstream consumer (including the series
        // this creates) sees the canonical key. The checker swaps the
        // identifier's =pair= field and the projection emits the corrected
        // key, so the value is never touched and this can never introduce
        // floating-point error. The reference data is read once, and only
        // when a key in the batch would actually consult it.
        std::optional<ores::ore::market::fx_quote_convention_checker> fx_checker;
        if (std::any_of(data.begin(), data.end(), [](const auto& d) {
                return d.series_type == "FX" && d.metric == "RATE";
            }))
            fx_checker.emplace(known_pairs_ ? known_pairs_() :
                                              fetch_known_currency_pairs(auth_nats_));

        // parse_market_data already de-duplicated repeated (date, key)
        // pairs (last-line-wins) — see duplicate_policy. In error mode,
        // skip persisting this content rather than silently importing
        // data the caller asked to be told about instead.
        if (report.errors.empty()) {
            std::vector<domain::market_observation> observations;
            observations.reserve(data.size());
            for (const auto& d : data) {
                const auto named =
                    canonical_key(d.key, registry, fx_checker ? &*fx_checker : nullptr);
                // A key oresmd cannot name has no identity for the series to carry,
                // and the identity is what the catalog is keyed by, so the row is
                // reported and dropped rather than filed under a series nothing can
                // find.
                if (!named) {
                    resp.warnings.push_back(d.key + " = " + d.value +
                                            " skipped: oresmd names no series for this key.");
                    continue;
                }
                const auto series_type = named->decomposition.series_type;
                const auto metric = named->decomposition.metric;
                const auto qualifier = named->decomposition.qualifier;
                const auto point =
                    named->decomposition.point_id ? named->decomposition.point_id : d.point_id;

                if (named->canonical != d.key)
                    resp.warnings.push_back(
                        d.key + " = " + d.value + " -> " + named->canonical + " = " + d.value +
                        " (value unchanged): " +
                        (named->fx_pair_reversed ?
                             "reversed relative to refdata's canonical currency pair." :
                             "not the canonical spelling of this key."));

                const auto series =
                    find_or_create_series(series_type, metric, qualifier, named->uri);

                domain::market_observation obs;
                obs.id = gen();
                obs.tenant_id = ctx_.tenant_id();
                obs.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
                obs.series_id = series;
                obs.observation_datetime = std::chrono::sys_days{d.date};
                // A key that carries no point of its own takes the series
                // type's answer for its single point.
                obs.point_id = point.value_or(registry.default_point_for(series_type));
                // The file's own text, kept because the rows above hold the
                // canonical spelling rather than it.
                obs.key = d.key;
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
                // Fixing series: series_type=FIXING, metric=RATE, qualifier=index_name.
                // The index name is an identity of its own class, so it is read by
                // from_index_name() rather than by the ORE key grammar, and the URI
                // it projects to is the series' identity. A name no class can name
                // has none, so the row is reported and dropped like an unnameable
                // key above.
                const auto identifier = core::oresmd_projections::from_index_name(f.qualifier);
                if (!identifier) {
                    resp.warnings.push_back(f.qualifier +
                                            " skipped: oresmd names no series for this index "
                                            "name.");
                    continue;
                }
                const auto series =
                    find_or_create_series(std::string(fixing_series_type),
                                          "RATE",
                                          f.qualifier,
                                          core::oresmd_parser::to_uri(*identifier).value);

                domain::market_fixing fix;
                fix.id = gen();
                fix.tenant_id = ctx_.tenant_id();
                fix.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
                fix.series_id = series;
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
