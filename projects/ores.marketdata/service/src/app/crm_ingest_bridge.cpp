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
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.analytics.quant/domain/ccy_pair_input.hpp"
#include "ores.analytics.quant/domain/driver_quote.hpp"
#include "ores.analytics.quant/domain/staleness_policy.hpp"
#include "ores.analytics.quant/service/topology_builder.hpp"
#include "ores.refdata.api/domain/crm_driver_pair.hpp"
#include "ores.refdata.api/domain/crm_enabled_derived_pair.hpp"
#include "ores.refdata.api/domain/crm_topology_config.hpp"
#include "ores.refdata.core/repository/crm_driver_pair_repository.hpp"
#include "ores.refdata.core/repository/crm_enabled_derived_pair_repository.hpp"
#include "ores.refdata.core/repository/crm_topology_config_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <map>
#include <stdexcept>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

// A party's rate_engine never considers a rate fresh past this age, purely
// as the CRM-internal default; consumers query with their own tolerance
// in mind via the rate_status returned alongside every rate.
constexpr auto default_staleness_max_age = std::chrono::minutes(15);

} // namespace

crm_ingest_bridge::crm_ingest_bridge(ores::database::context ctx)
    : ctx_(std::move(ctx))
    , engines_(std::make_shared<const engines_map>()) {}

void crm_ingest_bridge::refresh() {
    ores::refdata::repository::crm_topology_config_repository config_repo;
    ores::refdata::repository::crm_driver_pair_repository driver_repo;
    ores::refdata::repository::crm_enabled_derived_pair_repository derived_repo;

    const auto configs = config_repo.read_latest_all_tenants(ctx_);
    const auto driver_pairs = driver_repo.read_latest_all_tenants(ctx_);

    // Group driver pairs by their owning config_id.
    std::map<boost::uuids::uuid, std::vector<const ores::refdata::domain::crm_driver_pair*>>
        pairs_by_config;
    for (const auto& p : driver_pairs)
        if (p.enabled)
            pairs_by_config[p.config_id].push_back(&p);

    auto new_engines = std::make_shared<engines_map>();

    // Track (tenant, party, config_id) -> the built named_engine's index so
    // the enabled-derived-pairs pass below can find it without a second
    // linear-name lookup.
    std::map<boost::uuids::uuid, std::pair<pair_key, std::size_t>> engine_by_config_id;

    for (const auto& cfg : configs) {
        if (!cfg.enabled)
            continue;

        const auto tenant_id_str = cfg.tenant_id.to_string();
        const auto party_id_str = boost::uuids::to_string(cfg.party_id);
        const pair_key key{tenant_id_str, party_id_str};

        const auto it = pairs_by_config.find(cfg.id);
        if (it == pairs_by_config.end() || it->second.empty()) {
            BOOST_LOG_SEV(lg(), info)
                << "CRM: config '" << cfg.name << "' (tenant=" << tenant_id_str
                << " party=" << party_id_str << ") has no enabled driver pairs -- skipping";
            continue;
        }

        std::vector<quant::domain::ccy_pair_input> edges;
        std::vector<std::string> majors;
        std::vector<std::pair<std::string, std::string>> configured_pairs;
        edges.reserve(it->second.size());
        for (const auto* p : it->second) {
            edges.push_back({p->base_currency_code, p->quote_currency_code, true});
            majors.push_back(p->base_currency_code);
            majors.push_back(p->quote_currency_code);
            configured_pairs.emplace_back(p->base_currency_code, p->quote_currency_code);
        }

        try {
            // Two enabled configs sharing a name for the same (tenant,
            // party) should be structurally impossible: the codegened
            // crm_topology_configs_party_id_name_uniq_idx partial unique
            // index (tenant_id, party_id, name) WHERE valid_to = infinity
            // already rejects a second active row with the same name at
            // write time, independent of `enabled`. If this ever fires
            // it means that invariant was violated some other way (a
            // direct DB write bypassing the repository, a migration
            // bug, ...) -- loudly reject the duplicate rather than
            // silently picking one, but isolate the failure to this one
            // config via the existing catch below, same as a broken
            // topology, so it can't take down every other party's CRM.
            const auto existing_it = new_engines->find(key);
            if (existing_it != new_engines->end() &&
                std::ranges::any_of(existing_it->second,
                                    [&](const auto& ne) { return ne.name == cfg.name; })) {
                throw std::logic_error(
                    "duplicate enabled crm_topology_config name '" + cfg.name +
                    "' for tenant=" + tenant_id_str + " party=" + party_id_str +
                    " -- this should be prevented by the DB's partial unique index");
            }

            auto topology =
                quant::service::topology_builder::build(edges, cfg.pivot_currency_code, majors);
            auto engine = std::make_shared<quant::service::rate_engine>(
                std::move(topology), quant::domain::staleness_policy{default_staleness_max_age});

            auto& named_engines = (*new_engines)[key];
            named_engines.push_back(
                named_engine{cfg.name, std::move(engine), std::move(configured_pairs)});
            engine_by_config_id.emplace(cfg.id, std::make_pair(key, named_engines.size() - 1));

            BOOST_LOG_SEV(lg(), info) << "CRM: built rate_engine for tenant=" << tenant_id_str
                                      << " party=" << party_id_str << " config='" << cfg.name
                                      << "' (" << it->second.size() << " driver pair(s))";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "CRM: failed to build topology for tenant=" << tenant_id_str
                << " party=" << party_id_str << " config='" << cfg.name << "': " << e.what();
        }
    }

    // Enabled derived pairs don't affect the topology (they're never
    // edges), only which pairs rates() below considers "configured" --
    // append them to whichever named engine their owning config built.
    const auto derived_pairs = derived_repo.read_latest_all_tenants(ctx_);
    for (const auto& d : derived_pairs) {
        if (!d.enabled)
            continue;
        const auto it = engine_by_config_id.find(d.config_id);
        if (it == engine_by_config_id.end())
            continue;
        const auto& [key, index] = it->second;
        (*new_engines)[key][index].configured_pairs.emplace_back(d.base_currency_code,
                                                                 d.quote_currency_code);
    }

    std::size_t total_engines = 0;
    for (const auto& [key, named_engines] : *new_engines)
        total_engines += named_engines.size();

    {
        std::lock_guard lock(engines_mutex_);
        engines_ = std::move(new_engines);
    }
    BOOST_LOG_SEV(lg(), info) << "CRM: refresh complete, " << total_engines << " active engine(s)";
}

void crm_ingest_bridge::update(const std::string& tenant_id_str,
                               const std::string& party_id_str,
                               const std::string& base_currency_code,
                               const std::string& quote_currency_code,
                               double rate,
                               std::chrono::system_clock::time_point observed_at) {
    const auto snap = snapshot();
    const auto it = snap->find(pair_key{tenant_id_str, party_id_str});
    if (it == snap->end())
        return; // no CRM configured for this (tenant, party)

    // A single tick may be a driver edge of several of this party's named
    // CRMs at once (e.g. EUR/USD in both "majors" and "exotics") -- feed
    // every one of them.
    for (const auto& named_engine : it->second) {
        try {
            named_engine.engine->update(quant::domain::driver_quote{
                base_currency_code, quote_currency_code, rate, observed_at});
        } catch (const std::invalid_argument&) {
            // Not a driver edge of this CRM's topology (or an unknown
            // currency) -- not every tick is configured in every CRM.
        }
    }
}

std::optional<quant::domain::derived_rate>
crm_ingest_bridge::rate(const std::string& tenant_id_str,
                        const std::string& party_id_str,
                        const std::string& crm_name,
                        const std::string& base_currency_code,
                        const std::string& quote_currency_code) const {
    const auto snap = snapshot();
    const auto it = snap->find(pair_key{tenant_id_str, party_id_str});
    if (it == snap->end())
        return std::nullopt;
    const auto named_it = std::ranges::find(it->second, crm_name, &named_engine::name);
    if (named_it == it->second.end())
        return std::nullopt;
    return named_it->engine->rate(base_currency_code, quote_currency_code);
}

std::vector<quant::domain::derived_rate>
crm_ingest_bridge::rates(const std::string& tenant_id_str,
                         const std::string& party_id_str,
                         const std::string& crm_name) const {
    const auto snap = snapshot();
    const auto it = snap->find(pair_key{tenant_id_str, party_id_str});
    if (it == snap->end())
        return {};
    const auto named_it = std::ranges::find(it->second, crm_name, &named_engine::name);
    if (named_it == it->second.end())
        return {};
    return named_it->engine->rates(named_it->configured_pairs);
}

std::vector<named_rate> crm_ingest_bridge::rates(const std::string& tenant_id_str,
                                                 const std::string& party_id_str) const {
    const auto snap = snapshot();
    const auto it = snap->find(pair_key{tenant_id_str, party_id_str});
    if (it == snap->end())
        return {};

    std::vector<named_rate> result;
    for (const auto& named_engine : it->second) {
        auto rates = named_engine.engine->rates(named_engine.configured_pairs);
        result.reserve(result.size() + rates.size());
        for (auto& r : rates)
            result.push_back(named_rate{named_engine.name, std::move(r)});
    }
    return result;
}

} // namespace ores::marketdata::service::app
