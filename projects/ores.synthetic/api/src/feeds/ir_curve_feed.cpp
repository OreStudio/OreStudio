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
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/ir_curve_tick_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <cctype>
#include <chrono>
#include <format>
#include <stdexcept>
#include <thread>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.ir_curve_feed");
    return instance;
}

// DEPOSIT/SWAP publish onto the yield subclass; FRA onto fra -- the two curve_role values
// curve_instrument_pricer treats as "point instrument" and "interval instrument" respectively
// (see ir_curve_template_entry's own doc comment).
ores::marketdata::domain::series_subclass subclass_for(const std::string& curve_role) {
    using ores::marketdata::domain::series_subclass;
    if (curve_role == "FRA")
        return series_subclass::fra;
    return series_subclass::yield;
}

} // namespace

ir_curve_feed::ir_curve_feed(
    ores::nats::service::client& nats,
    ores::utility::uuid::tenant_id tenant_id,
    boost::uuids::uuid party_id,
    std::string source_name,
    std::string nats_subject,
    std::string series_type,
    std::string metric,
    std::string qualifier,
    std::string role,
    std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess> process,
    double ticks_per_hour,
    std::vector<ir_curve_resolved_entry> entries)
    : nats_(nats)
    , tenant_id_(std::move(tenant_id))
    , party_id_(party_id)
    , source_name_(std::move(source_name))
    , nats_subject_(std::move(nats_subject))
    , series_type_(std::move(series_type))
    , metric_(std::move(metric))
    , qualifier_(std::move(qualifier))
    , role_(std::move(role))
    , process_(std::move(process))
    , ticks_per_hour_(ticks_per_hour)
    , entries_(std::move(entries)) {

    if (!process_)
        throw std::invalid_argument("ir_curve_feed: process must not be null");
    if (ticks_per_hour_ <= 0.0)
        throw std::invalid_argument("ir_curve_feed: ticks_per_hour must be positive");
    if (entries_.empty())
        throw std::invalid_argument("ir_curve_feed: entries must not be empty");
}

void ir_curve_feed::start() {
    using namespace std::chrono;

    const auto period_us =
        duration_cast<microseconds>(hours(1)) / static_cast<long long>(ticks_per_hour_);

    // See fx_spot_feed::start() -- stop_flag_ must not be reset here, and the sleep is sliced so
    // stop() is observed promptly even though the tick period itself can be minutes long.
    constexpr auto slice = milliseconds(100);

    while (!stop_flag_.load(std::memory_order_relaxed)) {
        auto remaining = duration_cast<microseconds>(period_us);
        while (remaining.count() > 0 && !stop_flag_.load(std::memory_order_relaxed)) {
            const auto nap = remaining < slice ? remaining : duration_cast<microseconds>(slice);
            std::this_thread::sleep_for(nap);
            remaining -= nap;
        }

        if (stop_flag_.load(std::memory_order_relaxed))
            break;

        process_->next();
        const auto now = system_clock::now();

        // A tick-loop thread has no caller to propagate an exception to --
        // an uncaught throw here would std::terminate() the whole service
        // process, taking down every other feed and NATS handler with it.
        // Log and skip this batch instead; the next tick tries again.
        try {
            for (const auto& e : entries_) {
                ores::marketdata::domain::ir_curve_tick tick;
                tick.tenant_id = tenant_id_;
                tick.party_id = party_id_;
                tick.series_type = series_type_;
                tick.metric = metric_;
                tick.qualifier = qualifier_;
                tick.subclass = subclass_for(e.curve_role);
                tick.point_id = e.point_id;
                tick.source_name = source_name_;
                tick.datetime = now;
                tick.value = price_ir_curve_entry(*process_, e);

                nats_.js_publish(nats_subject_, ores::nats::default_wire_codec().encode(tick));
            }

            const auto n = publish_count_.fetch_add(1, std::memory_order_relaxed) + 1;
            if (n == 1 || n % 100 == 0) {
                BOOST_LOG_SEV(lg(), info)
                    << "SYNTHETIC CURVE PUBLISH: subject='" << nats_subject_ << "' source='"
                    << source_name_ << "' batch=" << n << " points=" << entries_.size();
            }
        } catch (const std::exception& ex) {
            BOOST_LOG_SEV(lg(), error)
                << "SYNTHETIC CURVE PUBLISH FAILED: subject='" << nats_subject_ << "' source='"
                << source_name_ << "': " << ex.what();
        }
    }
}

void ir_curve_feed::stop() {
    stop_flag_.store(true, std::memory_order_relaxed);
}

ORES_SYNTHETIC_API_EXPORT const ir_curve_resolved_entry*
select_vintage_anchor_entry(const std::vector<ir_curve_resolved_entry>& resolved) {
    const ir_curve_resolved_entry* anchor = nullptr;
    for (const auto& e : resolved) {
        if (e.curve_role != "DEPOSIT")
            continue;
        if (!anchor || e.ticks_ahead_end < anchor->ticks_ahead_end)
            anchor = &e;
    }
    return anchor;
}

namespace {
std::string lowercase(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return s;
}

// ISO date part of an observation_datetime -- see feed_controller::date_part()'s own copy of this
// (duplicated rather than shared: the two live in different components with no natural common
// header for a one-line helper).
std::string date_part(std::chrono::system_clock::time_point tp) {
    const auto days = std::chrono::floor<std::chrono::days>(tp);
    return std::format("{:%F}", days);
}

// Resolves initial_rate from a real market_observation when cfg.price_source is "vintage",
// mirroring feed_controller::vintage_data_available() -- but keyed on the resolved entries'
// shortest-tenor DEPOSIT entry's point_id rather than a hardcoded "SPOT", since an IR curve feed
// has no single scalar equivalent to FX spot (see make_ir_curve_feed's own doc comment for why
// DEPOSIT is the anchor).
//
// @throws vintage_data_missing_error if there is no DEPOSIT entry to anchor on, or no matching
// observation is found.
double resolve_vintage_initial_rate(ores::nats::service::nats_client& auth_nats,
                                    const ores::synthetic::domain::ir_curve_generation_config& cfg,
                                    const std::vector<ir_curve_resolved_entry>& resolved,
                                    const std::string& caller_bearer_token) {
    const auto* anchor = select_vintage_anchor_entry(resolved);
    if (!anchor) {
        throw vintage_data_missing_error(
            "Cannot resolve vintage initial_rate: config has no DEPOSIT entry to anchor on.");
    }

    const auto missing_message = [&] {
        return "No vintage data found for source=" + cfg.vintage_source +
               ", date=" + cfg.vintage_date + ", point_id=" + anchor->point_id + ".";
    };

    const auto qualifier = ir_curve_qualifier(cfg);

    auto delegated_nats = auth_nats.with_delegation(caller_bearer_token);
    ores::marketdata::client::market_data_client md_client(delegated_nats);

    auto series = md_client.find_series("RATES", "YIELD", qualifier);
    if (!series)
        throw vintage_data_missing_error("Failed to look up series for '" + qualifier +
                                         "': " + series.error());
    if (!series->has_value())
        throw vintage_data_missing_error(missing_message());

    // Paged scan -- see feed_controller::vintage_data_available()'s own comment on why an
    // unbounded fetch is unsafe here (NATS max payload).
    constexpr std::uint32_t page_size = 200;
    const auto series_id_str = boost::uuids::to_string((*series)->id);
    std::uint32_t offset = 0;
    for (;;) {
        auto observations = md_client.list_observations_page(series_id_str, offset, page_size);
        if (!observations) {
            throw vintage_data_missing_error("Failed to look up observations for '" + qualifier +
                                             "': " + observations.error());
        }
        for (const auto& obs : *observations) {
            if (obs.source == cfg.vintage_source && obs.point_id == anchor->point_id &&
                date_part(obs.observation_datetime) == cfg.vintage_date) {
                try {
                    return std::stod(obs.value);
                } catch (const std::exception& e) {
                    throw vintage_data_missing_error("Vintage observation value '" + obs.value +
                                                     "' is not a valid number: " + e.what());
                }
            }
        }
        if (observations->size() < page_size)
            break;
        offset += page_size;
    }
    throw vintage_data_missing_error(missing_message());
}

}

std::shared_ptr<ir_curve_feed> make_ir_curve_feed(
    ores::nats::service::client& nats,
    ores::nats::service::nats_client& auth_nats,
    const ores::synthetic::domain::ir_curve_generation_config& cfg,
    const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
    const std::vector<ores::synthetic::domain::ir_curve_generation_config_process_parameter_value>&
        values,
    const std::vector<ores::synthetic::domain::yield_curve_process_parameter_definition>&
        definitions,
    const ir_curve_refdata_context& refctx,
    const std::string& caller_bearer_token) {
    auto resolved = resolve(entries, refctx, cfg.fixed_leg_payment_frequency_code);

    // "vintage" resolves the initial_rate parameter from a real market_observation, overriding
    // the stored value row before mapping; "fixed" (the default) uses the stored value as-is.
    // See the field's own doc comment for the vintage semantics.
    auto cfg_values = values;
    if (cfg.price_source == "vintage") {
        const auto initial_rate =
            resolve_vintage_initial_rate(auth_nats, cfg, resolved, caller_bearer_token);
        const auto def_it =
            std::find_if(definitions.begin(), definitions.end(), [&](const auto& d) {
                return lowercase(d.process_type_code) == lowercase(cfg.process_type) &&
                       d.parameter_name == "initial_rate";
            });
        if (def_it == definitions.end())
            throw std::invalid_argument("make_ir_curve_feed: process type '" + cfg.process_type +
                                        "' has no parameter definition for 'initial_rate'");
        for (auto& v : cfg_values) {
            if (v.parameter_definition_id == def_it->id) {
                v.parameter_value = initial_rate;
                break;
            }
        }
    }

    auto process = ores::synthetic::domain::map_parameters_to_yield_curve_process(
        cfg.process_type, definitions, cfg_values, 42, ir_curve_feed_dt);

    // source_name is a persisted, editable column (see the field's own doc comment) -- the same
    // shape fx_spot_generation_config.source_name already uses, set at publish/save time rather
    // than computed here.
    return std::make_shared<ir_curve_feed>(
        nats,
        cfg.tenant_id,
        cfg.party_id,
        cfg.source_name,
        ores::marketdata::domain::synthetic_tick_subject(ir_curve_feed_kind, cfg.source_name),
        "RATES",
        "YIELD",
        ir_curve_qualifier(cfg),
        cfg.role,
        std::move(process),
        static_cast<double>(cfg.ticks_per_hour),
        std::move(resolved));
}

}
