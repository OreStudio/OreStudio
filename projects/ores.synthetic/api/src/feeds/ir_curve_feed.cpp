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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.synthetic.api/feeds/producer_subject.hpp"
#include "ores.synthetic.api/feeds/vintage_lookup.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <format>
#include <stdexcept>

namespace ores::synthetic::feed {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.api.ir_curve_feed");
    return instance;
}

} // namespace

ir_curve_feed::ir_curve_feed(
    ores::nats::service::client& nats,
    std::string source_name,
    std::string nats_subject,
    std::string qualifier,
    std::string role,
    std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess> process,
    double ticks_per_hour,
    std::vector<ir_curve_resolved_entry> entries)
    : nats_(nats)
    , source_name_(std::move(source_name))
    , nats_subject_(std::move(nats_subject))
    , qualifier_(std::move(qualifier))
    , role_(std::move(role))
    , process_(std::move(process))
    , clock_(ticks_per_hour)
    , entries_(std::move(entries)) {

    if (!process_)
        throw std::invalid_argument("ir_curve_feed: process must not be null");
    if (ticks_per_hour <= 0.0)
        throw std::invalid_argument("ir_curve_feed: ticks_per_hour must be positive");
    if (entries_.empty())
        throw std::invalid_argument("ir_curve_feed: entries must not be empty");
}

void ir_curve_feed::start() {
    clock_.run(
        [this] {
            process_->next();
            const auto now = std::chrono::system_clock::now();

            for (const auto& e : entries_) {
                // Each tick names one pillar: the datum the resolver builds from
                // the entry's own dates, so the series it lands in carries the
                // instrument's identity rather than the pipeline's vocabulary.
                const auto ccy = qualifier_.substr(0, qualifier_.find('/'));
                const auto key = ores::marketdata::core::make_pillar_quote_key(
                    ccy, e.start_tenor_code, e.start_date, e.end_date);
                ores::marketdata::messaging::market_tick tick;
                tick.oresmd_uri = ores::marketdata::core::pillar_datum_uri(key);
                tick.value = std::format("{}", price_ir_curve_entry(*process_, e));
                tick.observation_time = now;
                tick.source = source_name_;

                nats_.js_publish(nats_subject_, ores::nats::default_wire_codec().encode(tick));
            }
            return std::to_string(entries_.size());
        },
        [this](std::uint64_t n, const std::string& points) {
            if (n == 1 || n % 100 == 0) {
                BOOST_LOG_SEV(lg(), info)
                    << "SYNTHETIC CURVE PUBLISH: subject='" << nats_subject_ << "' source='"
                    << source_name_ << "' batch=" << n << " points=" << points;
            }
        },
        [this](const std::exception& ex) {
            BOOST_LOG_SEV(lg(), error)
                << "SYNTHETIC CURVE PUBLISH FAILED: subject='" << nats_subject_ << "' source='"
                << source_name_ << "': " << ex.what();
        });
}

void ir_curve_feed::stop() {
    clock_.stop();
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

// Resolves initial_rate from a real market_observation when cfg.price_source is "vintage",
// keyed on the resolved entries' shortest-tenor DEPOSIT entry rather than any one coordinate,
// since an IR curve feed has no single scalar equivalent to FX spot (see make_ir_curve_feed's
// own doc comment for why DEPOSIT is the anchor).
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

    const auto missing_message = "No vintage data found for source=" + cfg.vintage_source +
                                 ", date=" + cfg.vintage_date + ", point=" + anchor->point_id + ".";

    // The row the vintage is read from is the anchor pillar's own datum: the series
    // the config names at the pillar's point. The codecs compose both, so the read
    // and the write agree without either side decomposing the key by hand.
    namespace datum = ores::marketdata::datum;
    const auto series_datum = datum::oresmd_uri_codec::read(cfg.vintage_series_uri);
    if (!series_datum || !series_datum->is_series())
        throw vintage_data_missing_error("'" + cfg.vintage_series_uri + "' is not a series URI" +
                                         (series_datum ? "" : ": " + series_datum.error()));
    const auto anchor_datum = datum::datum_at_point(*series_datum, anchor->point_id);
    if (!anchor_datum)
        throw vintage_data_missing_error("no datum for series '" + cfg.vintage_series_uri +
                                         "' at point '" + anchor->point_id +
                                         "': " + anchor_datum.error());
    const auto anchor_uri = datum::oresmd_uri_codec::write(*anchor_datum);
    if (!anchor_uri)
        throw vintage_data_missing_error("no URI for series '" + cfg.vintage_series_uri +
                                         "' at point '" + anchor->point_id +
                                         "': " + anchor_uri.error());

    const auto found = find_vintage_observation(auth_nats,
                                                caller_bearer_token,
                                                cfg.vintage_series_uri,
                                                *anchor_uri,
                                                cfg.vintage_source,
                                                cfg.vintage_date,
                                                missing_message,
                                                cfg.vintage_series_uri,
                                                boost::uuids::to_string(cfg.party_id));
    if (!found)
        throw vintage_data_missing_error(found.error());
    return *found;
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
    ores::synthetic::domain::binding_mode binding_mode,
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
    return std::make_shared<ir_curve_feed>(nats,
                                           cfg.source_name,
                                           producer_subject(cfg.source_name, binding_mode),
                                           ir_curve_qualifier(cfg),
                                           cfg.role,
                                           std::move(process),
                                           static_cast<double>(cfg.ticks_per_hour),
                                           std::move(resolved));
}

}
