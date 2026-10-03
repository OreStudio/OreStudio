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
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
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
#include <expected>
#include <functional>
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

// What the import makes of one key: the datum it names, with an FX pair the
// checker found reversed already corrected, and the key, series URI and datum
// URI that datum writes as.
struct named_key final {
    datum::market_datum datum;
    std::string canonical;
    std::string series_uri;
    std::string datum_uri;
    bool fx_pair_reversed = false;
};

// The checker, read on first use: an import with no FX spot rate never reads
// the reference data.
using fx_checker_source = std::function<const ores::ore::market::fx_quote_convention_checker&()>;

// The datum with its FX spot pair put the way refdata knows it. Only an FX spot
// rate is checked, as the vendored ORE files that store a pair reversed only do
// so there; the value is never touched. Refdata holds upper-case codes, so a
// pair written in another case is left as written rather than guessed at.
bool correct_fx_pair(datum::market_datum& d, const fx_checker_source& fx_checker) {
    using datum::field;
    if (d.type() != datum::instrument_type::fx_spot || d.quote() != datum::quote_type::rate)
        return false;
    const auto& checker = fx_checker();
    const auto unit = *d.get<field::unit_ccy>();
    const auto ccy = *d.get<field::ccy>();
    const auto result = checker.check(unit, ccy);
    if (result.base_currency == unit && result.quote_currency == ccy)
        return false;
    d = datum::market_datum::make(
            d.type(),
            d.quote(),
            {{field::unit_ccy, result.base_currency}, {field::ccy, result.quote_currency}})
            .value();
    return true;
}

// The ORE key codec is the authority for what an ORE key means. A key it reads is
// stored under the canonical spelling its datum writes, so two spellings of one
// instrument reach one series; a key it refuses returns the reason, and the
// caller drops the row rather than filing it under a series with no identity.
std::expected<named_key, std::string> name_key(const std::string& key,
                                               const fx_checker_source& fx_checker) {
    auto read = datum::ore_key_codec::read(key);
    if (!read)
        return std::unexpected(read.error());
    named_key result{std::move(*read), {}, {}, {}, false};
    result.fx_pair_reversed = correct_fx_pair(result.datum, fx_checker);
    auto canonical = datum::ore_key_codec::write(result.datum);
    auto datum_uri = datum::oresmd_uri_codec::write(result.datum);
    auto series_uri = datum::oresmd_uri_codec::write(datum::series_of(result.datum));
    if (!canonical || !datum_uri || !series_uri)
        return std::unexpected("the datum this key names has no canonical spelling");
    result.canonical = std::move(*canonical);
    result.datum_uri = std::move(*datum_uri);
    result.series_uri = std::move(*series_uri);
    return result;
}

// The three facts the classification rules are keyed by, from the datum: the
// key's first token, its quote token and, for a correlation, its two indices.
struct classification_key final {
    std::string series_type;
    std::string metric;
    std::string qualifier;
};

classification_key classification_key_of(const datum::market_datum& d) {
    classification_key k{std::string(datum::ore_key_codec::token_of(d.type())),
                         std::string(datum::ore_name(d.quote())),
                         {}};
    if (d.type() == datum::instrument_type::correlation)
        k.qualifier = *d.get<datum::field::index1>() + "/" + *d.get<datum::field::index2>();
    return k;
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

    // The identity the series is given: the series URI of the datum its key names,
    // or the oresmd URI of its index name. Every caller has one, because a row the
    // codecs cannot name is not imported at all.
    auto find_or_create_series = [&](const std::string& series_type,
                                     const std::string& metric,
                                     const std::string& qualifier,
                                     const std::string& oresmd_uri) -> boost::uuids::uuid {
        // The identity is what the series is, so the cache is keyed by it.
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

        // Best-effort: a reversed FX spot key (some vendored ORE example
        // market.txt files store GBP/USD under FX/RATE/USD/GBP; see
        // fx_quote_convention_checker) is corrected against ores.refdata's
        // currency pairs before persistence, so the series and the row hold
        // the corrected pair only. The reference data is read once, on the
        // first datum that is an FX spot rate.
        std::optional<ores::ore::market::fx_quote_convention_checker> fx_checker;
        const fx_checker_source checker_source =
            [&]() -> const ores::ore::market::fx_quote_convention_checker& {
            if (!fx_checker)
                fx_checker.emplace(known_pairs_ ? known_pairs_() :
                                                  fetch_known_currency_pairs(auth_nats_));
            return *fx_checker;
        };

        // parse_market_data already de-duplicated repeated (date, key)
        // pairs (last-line-wins) -- see duplicate_policy. In error mode,
        // skip persisting this content rather than silently importing
        // data the caller asked to be told about instead.
        if (report.errors.empty()) {
            std::vector<domain::market_observation> observations;
            observations.reserve(data.size());
            for (const auto& d : data) {
                const auto named = name_key(d.key, checker_source);
                if (!named) {
                    resp.warnings.push_back(d.key + " = " + d.value + " skipped: " + named.error());
                    continue;
                }

                if (named->canonical != d.key)
                    resp.warnings.push_back(
                        d.key + " = " + d.value + " -> " + named->canonical + " = " + d.value +
                        " (value unchanged): " +
                        (named->fx_pair_reversed ?
                             "reversed relative to refdata's canonical currency pair." :
                             "not the canonical spelling of this key."));

                const auto ck = classification_key_of(named->datum);
                const auto series = find_or_create_series(
                    ck.series_type, ck.metric, ck.qualifier, named->series_uri);

                domain::market_observation obs;
                obs.id = gen();
                obs.tenant_id = ctx_.tenant_id();
                obs.party_id = ctx_.party_id().value_or(boost::uuids::uuid{});
                obs.series_id = series;
                obs.observation_datetime = std::chrono::sys_days{d.date};
                obs.oresmd_uri = named->datum_uri;
                // The canonical key of the datum the row holds, never the file's
                // own text: a row names one identity.
                obs.key = named->canonical;
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
