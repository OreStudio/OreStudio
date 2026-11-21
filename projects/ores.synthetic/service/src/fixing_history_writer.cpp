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
#include "fixing_history_writer.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.synthetic.api/feeds/index_fixings.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <vector>

namespace ores::synthetic::service {

using namespace ores::logging;

namespace {

/// The provider the run's own fixings are kept under, beside the published ones.
constexpr std::string_view derived_provider = "derived";

/// The subclass of an index's fixing series, as the reference data names it.
constexpr std::string_view index_fixing_subclass = "index_fixing";

/// The derivation the reference data names for a curve's output, which a fixing history is.
constexpr std::string_view ir_curve_bootstrap = "IR_CURVE_BOOTSTRAP";

/// The version of the derivation this run states, as the series records it.
constexpr int derivation_version = 1;

/// How many days of a curve's index a run states, from the as-of back.
constexpr int fixing_history_days = 520;

/// How many fixings one write carries.
constexpr std::size_t fixing_write_batch = 500;

/// How the writer waits for a market data service that does not answer.
ores::service::service::reconciler_options writer_reconciler() {
    return {.name = "fixing history",
            .first_delay = std::chrono::seconds(1),
            .max_delay = std::chrono::seconds(60)};
}

/// How wide a window the read of the days already held asks for.
constexpr int held_window_days = 800;

}

auto& fixing_history_writer::lg() {
    static auto instance = make_logger("ores.synthetic.service.fixing_history_writer");
    return instance;
}

fixing_history_writer::fixing_history_writer(const ores::database::context& db_ctx,
                                             ores::nats::service::nats_client& svc_nats)
    : db_ctx_(db_ctx)
    , svc_nats_(svc_nats)
    , reconciler_(writer_reconciler(), [this] { return write_pending(); }) {}

fixing_history_writer::~fixing_history_writer() {
    stop();
}

void fixing_history_writer::enqueue(feed::ir_curve_feed::fixing_plan plan) {
    {
        std::lock_guard lock(mu_);
        auto key = plan.index_uri;
        pending_[std::move(key)] = {.plan = std::move(plan), .generation = ++generation_};
    }
    reconciler_.kick();
}

void fixing_history_writer::stop() {
    reconciler_.stop();
}

ores::service::service::reconcile_result fixing_history_writer::write_pending() {
    std::map<std::string, pending_plan> snapshot;
    {
        std::lock_guard lock(mu_);
        snapshot = pending_;
    }

    std::size_t left = 0;
    std::string first_reason;
    for (const auto& [index_uri, entry] : snapshot) {
        std::expected<void, std::string> written;
        try {
            written = write(entry.plan);
        } catch (const std::exception& e) {
            written = std::unexpected(std::string(e.what()));
        }
        if (!written) {
            ORES_LOG_SEV(lg(), debug)
                << "The fixings of '" << index_uri << "' are not written: " << written.error();
            if (left++ == 0)
                first_reason =
                    "the fixings of '" + index_uri + "' are not written: " + written.error();
            continue;
        }
        std::lock_guard lock(mu_);
        const auto it = pending_.find(index_uri);
        if (it != pending_.end() && it->second.generation == entry.generation)
            pending_.erase(it);
    }

    if (left == 0)
        return ores::service::service::reconcile_result::finished();
    return ores::service::service::reconcile_result::pending(std::to_string(left) +
                                                             " plan(s) left; " + first_reason);
}

std::expected<boost::uuids::uuid, std::string> fixing_history_writer::system_party() {
    if (!system_party_) {
        ores::refdata::repository::party_repository parties;
        const auto system = parties.read_system_party(db_ctx_, db_ctx_.tenant_id().to_string());
        if (system.empty())
            return std::unexpected("The tenant has no system party.");
        system_party_ = system.front().id;
    }
    return *system_party_;
}

std::expected<boost::uuids::uuid, std::string>
fixing_history_writer::series_for(const std::string& index_uri, const boost::uuids::uuid& party) {
    ores::marketdata::client::market_data_client md(svc_nats_);
    const auto party_text = boost::uuids::to_string(party);
    const auto found = md.find_series_by_uri(index_uri, party_text);
    if (!found)
        return std::unexpected(found.error());
    if (!found->empty())
        return found->front().id;

    // The series the run's fixings are kept in, beside whatever a publisher keeps for the same
    // index. A fixing series is a term structure of one index, so the identity of the series is
    // the address of that index.
    boost::uuids::random_generator ids;
    const auto written = md.put_series(
        {.id = ids(),
         .party_id = party,
         .oresmd_uri = index_uri,
         .provider_code = std::string(derived_provider),
         .series_subclass = std::string(index_fixing_subclass),
         // The history is a stand-in a curve states, so the series says what
         // derived it. The index's own address decides the config id, so a
         // re-run names the same derivation.
         .derivation_kind = std::string(ir_curve_bootstrap),
         .derivation_config_id = boost::uuids::name_generator_sha1(boost::uuids::uuid{})(index_uri),
         .derivation_config_version = derivation_version},
        {.reason_code = "system.new_record",
         .commentary = "The series the simulator's fixing history is kept in"});
    if (!written)
        return std::unexpected(written.error());
    return written->id;
}

std::set<std::chrono::year_month_day> fixing_history_writer::held_days(
    const std::string& index_uri, const boost::uuids::uuid& party, std::chrono::year_month_day to) {
    // Reads go delegated, as every read of the catalogue does, because a service's own session
    // holds no party.
    auto delegated = svc_nats_.with_delegation({});
    ores::marketdata::client::market_data_client md(delegated);
    ores::marketdata::messaging::get_fixing_window_request request;
    request.item = index_uri;
    request.party_id = boost::uuids::to_string(party);
    request.to = to;
    request.days = held_window_days;
    const auto window = md.get_fixing_window(request);
    if (!window) {
        // A series the market holds nothing for is refused rather than answered empty, which is
        // the same thing to a caller that wants the days a value is not already held on.
        ORES_LOG_SEV(lg(), debug) << "The days the market holds of '" << index_uri
                                  << "' are not read: " << window.error();
        return {};
    }
    return {window->dates.begin(), window->dates.end()};
}

std::expected<void, std::string>
fixing_history_writer::write(const feed::ir_curve_feed::fixing_plan& plan) {
    const auto started = std::chrono::steady_clock::now();
    const auto party = system_party();
    if (!party)
        return std::unexpected(party.error());
    const auto held = held_days(plan.index_uri, *party, plan.as_of);
    const auto history = feed::derive_fixing_history(
        plan.level, plan.as_of, fixing_history_days, plan.run_seed, plan.index_uri, held);
    if (!history) {
        ORES_LOG_SEV(lg(), warn) << "The fixings of '" << plan.index_uri
                                 << "' are not stated: " << history.error();
        return {};
    }
    if (history->empty())
        return {};

    const auto series = series_for(plan.index_uri, *party);
    if (!series)
        return std::unexpected("it has no series: " + series.error());

    ores::marketdata::client::market_data_client md(svc_nats_);
    boost::uuids::random_generator ids;
    std::vector<ores::marketdata::messaging::market_fixing_write> batch;
    std::size_t written = 0;
    std::string store_error;
    const auto flush = [&]() -> bool {
        if (batch.empty())
            return true;
        const auto stored =
            md.put_fixings(batch,
                           {.reason_code = "system.new_record",
                            .commentary = "The fixings a simulated run states for its curves"});
        batch.clear();
        if (!stored) {
            store_error = "they are not stored: " + stored.error();
            return false;
        }
        written += *stored;
        return true;
    };
    for (const auto& fixing : *history) {
        batch.push_back({.id = ids(),
                         .party_id = *party,
                         .series_id = *series,
                         .fixing_date = fixing.date,
                         .value = fixing.value,
                         .source = "synthetic"});
        if (batch.size() >= fixing_write_batch && !flush())
            return std::unexpected(store_error);
    }
    if (!flush())
        return std::unexpected(store_error);
    ORES_LOG_SEV(lg(), info)
        << "Stated " << written << " fixing(s) of '" << plan.index_uri << "' to " << plan.as_of
        << " in "
        << std::chrono::duration<double>(std::chrono::steady_clock::now() - started).count()
        << "s, of " << history->size() << " day(s) the market did not hold";
    return {};
}

}
