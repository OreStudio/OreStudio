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
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
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
    std::string currency_code,
    std::string index_tenor,
    std::string role,
    std::unique_ptr<ores::analytics::quant::domain::i_yield_curve_process> process,
    const ores::analytics::quant::domain::arrival_spec& arrival,
    std::vector<ir_curve_resolved_entry> entries,
    ores::analytics::quant::domain::market_link link,
    ores::analytics::quant::service::tick_scheduler* scheduler)
    : nats_(nats)
    , source_name_(std::move(source_name))
    , nats_subject_(std::move(nats_subject))
    , currency_code_(std::move(currency_code))
    , role_(std::move(role))
    , process_(std::move(process))
    , arrivals_(ores::analytics::quant::service::process_factory::make_arrival_process(arrival))
    , clock_(*arrivals_,
             ores::analytics::quant::domain::with_mean_rate(link, arrival.ticks_per_hour))
    , ticks_per_second_(arrival.ticks_per_hour / 3600.0)
    , intensity_(link.intensity)
    , arrival_(arrival)
    , link_(link)
    , scheduler_(scheduler)
    , entries_(std::move(entries)) {

    if (!std::isfinite(link.sensitivity) || link.sensitivity <= 0.0)
        throw std::invalid_argument(
            "ir_curve_feed: sensitivity must be finite and strictly positive");
    if (!process_)
        throw std::invalid_argument("ir_curve_feed: process must not be null");
    if (arrival.ticks_per_hour <= 0.0)
        throw std::invalid_argument("ir_curve_feed: ticks_per_hour must be positive");
    if (entries_.empty())
        throw std::invalid_argument("ir_curve_feed: entries must not be empty");

    display_subject_ = ores::marketdata::domain::feed_display_subject_for(source_name_);

    // Each pillar is a quote of one series. The keys are built once, here, so a pillar the
    // codec refuses fails when the feed is built and not on the first tick.
    for (const auto& e : entries_) {
        const auto key = ores::marketdata::core::make_pillar_quote_key(
            currency_code_, index_tenor, e.start_tenor_code, e.start_date, e.end_date, e.point_id);
        datum_uris_.push_back(ores::marketdata::core::pillar_datum_uri(key));
        auto series = ores::marketdata::core::pillar_series_uri(key);
        if (std::ranges::find(series_uris_, series) == series_uris_.end())
            series_uris_.push_back(std::move(series));
    }
}

void ir_curve_feed::start() {
    const auto body = [this](double elapsed_seconds) {
        return tick(elapsed_seconds);
    };

    // A scheduler, when the run has one, decides when this feed ticks: it holds
    // every series' next gap, so registering is all start() does.
    if (scheduler_ != nullptr) {
        handle_ = scheduler_->add(
            *arrivals_,
            ores::analytics::quant::domain::with_mean_rate(link_, arrival_.ticks_per_hour),
            body);
        return;
    }

    clock_.run(
        body,
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

std::string ir_curve_feed::tick(double elapsed_seconds) {
    published_.fetch_add(1, std::memory_order_relaxed);

    const auto elapsed_ticks = elapsed_seconds > 0.0 ? elapsed_seconds * ticks_per_second_ : 1.0;
    process_->next(elapsed_ticks);
    if (intensity_ != nullptr)
        intensity_->record_event();
    const auto now = std::chrono::system_clock::now();

    for (std::size_t i = 0; i < entries_.size(); ++i) {
        const auto& e = entries_[i];
        // Each tick names one pillar: the datum built at construction from the
        // entry's own dates and the index tenor, so the series it lands in
        // carries the instrument's identity rather than the pipeline's
        // vocabulary.
        ores::marketdata::messaging::market_tick tick;
        tick.oresmd_uri = datum_uris_[i];
        tick.value = std::format("{}", price_ir_curve_entry(*process_, e));
        tick.observation_time = now;
        tick.source = source_name_;

        const auto encoded = ores::nats::default_wire_codec().encode(tick);
        nats_.publish(nats_subject_, encoded);
        nats_.publish(display_subject_, encoded);
    }
    return std::to_string(entries_.size());
}

void ir_curve_feed::stop() {
    if (scheduler_ != nullptr && handle_) {
        scheduler_->remove(*handle_);
        handle_.reset();
    }
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
                                    const std::string& start_date) {
    const auto* anchor = select_vintage_anchor_entry(resolved);
    if (!anchor) {
        throw vintage_data_missing_error(
            "Cannot resolve vintage initial_rate: config has no DEPOSIT entry to anchor on.");
    }

    const auto missing_message = "No vintage data found for source=" + cfg.vintage_source +
                                 ", date=" + start_date + ", point=" + anchor->point_id + ".";

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
                                                {},
                                                cfg.vintage_series_uri,
                                                *anchor_uri,
                                                cfg.vintage_source,
                                                start_date,
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
    const ores::marketdata::domain::market_clock* clock,
    ores::analytics::quant::domain::market_link link,
    ores::analytics::quant::service::tick_scheduler* scheduler) {
    auto resolved = resolve(entries, refctx, cfg.fixed_leg_payment_frequency_code);

    // "vintage" resolves the initial_rate parameter from a real market_observation, overriding
    // the stored value row before mapping; "fixed" (the default) uses the stored value as-is.
    // See the field's own doc comment for the vintage semantics.
    auto cfg_values = values;
    if (cfg.price_source == "vintage") {
        const auto initial_rate = resolve_vintage_initial_rate(
            auth_nats,
            cfg,
            resolved,
            ores::marketdata::domain::start_date(cfg.vintage_date, clock));
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
    const auto arrival = to_arrival_spec(cfg);
    const auto compatibility = ores::analytics::quant::domain::validate_step_compatibility(
        arrival.mode, process->supports_variable_step());
    if (!compatibility.valid)
        throw std::invalid_argument("make_ir_curve_feed: " + compatibility.message);

    return std::make_shared<ir_curve_feed>(nats,
                                           cfg.source_name,
                                           producer_subject(cfg.source_name),
                                           cfg.currency_code,
                                           ir_curve_index_tenor(cfg),
                                           cfg.role,
                                           std::move(process),
                                           arrival,
                                           std::move(resolved),
                                           link,
                                           scheduler);
}

}
