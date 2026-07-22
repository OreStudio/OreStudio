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
#include "ir_curve_feed.hpp"
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/ir_curve_tick_json_io.hpp" // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp"                      // IWYU pragma: keep.
#include <algorithm>
#include <cctype>
#include <chrono>
#include <rfl/json.hpp>
#include <span>
#include <stdexcept>
#include <thread>

namespace ores::synthetic::service {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.ir_curve_feed");
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

        // A tick-loop thread has no caller to propagate an exception to (see
        // curve_feed_controller::add()) -- an uncaught throw here would std::terminate() the
        // whole service process, taking down every other feed and NATS handler with it. Log and
        // skip this batch instead; the next tick tries again.
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

                const auto json = rfl::json::write(tick);
                const auto data = std::as_bytes(std::span{json.data(), json.size()});
                nats_.js_publish(nats_subject_, data);
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

namespace {
std::string lowercase(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return s;
}

// index_name is stored as the full floating_index_type code (e.g. "USD-SOFR"), which already
// bakes in currency_code -- strip that redundant "<CCY>-" prefix back off for display/subject
// purposes (see the field's own doc comment on ir_curve_generation_config for why it's stored
// with the prefix in the first place: reusing floating_index_type's existing single-argument
// validator rather than a bespoke composite one).
std::string strip_currency_prefix(const std::string& currency_code, const std::string& index_name) {
    const auto prefix = currency_code + "-";
    if (index_name.starts_with(prefix))
        return index_name.substr(prefix.size());
    return index_name;
}
}

std::shared_ptr<ir_curve_feed>
make_ir_curve_feed(ores::nats::service::client& nats,
                   const ores::synthetic::domain::ir_curve_generation_config& cfg,
                   const std::vector<ores::synthetic::domain::ir_curve_template_entry>& entries,
                   const ir_curve_refdata_context& refctx) {
    auto resolved = resolve(entries, refctx, cfg.fixed_leg_payment_frequency_code);

    // 1 tick == 1 calendar day (see ir_curve_template_resolver.hpp's own doc comment for this
    // convention) -- dt is real elapsed years per tick, not a caller-side kappa/sigma pre-scale.
    constexpr double dt = 1.0 / 365.0;
    auto process = ores::analytics::quant::service::process_factory::make_yield_curve_process(
        lowercase(cfg.process_type), cfg.kappa, {cfg.theta}, cfg.sigma, cfg.initial_rate, 42, dt);

    // source_name is a persisted, editable column (see the field's own doc comment) -- the same
    // shape fx_spot_generation_config.source_name already uses, set at publish/save time rather
    // than computed here.
    return std::make_shared<ir_curve_feed>(
        nats,
        cfg.tenant_id,
        cfg.party_id,
        cfg.source_name,
        "synthetic.v1.curve_family." + cfg.source_name,
        "RATES",
        "YIELD",
        cfg.currency_code + "/" + strip_currency_prefix(cfg.currency_code, cfg.index_name),
        std::move(process),
        static_cast<double>(cfg.ticks_per_hour),
        std::move(resolved));
}

}
