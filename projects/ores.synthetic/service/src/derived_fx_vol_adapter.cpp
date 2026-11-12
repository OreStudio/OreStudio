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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "derived_fx_vol_adapter.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.refdata.core/repository/fx_option_convention_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/uuid/name_generator.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <charconv>
#include <format>
#include <optional>
#include <set>
#include <span>
#include <utility>

namespace ores::synthetic::service {

using namespace ores::logging;
using clock = std::chrono::steady_clock;
namespace mm = ores::marketdata::messaging;
namespace subjects = ores::marketdata::domain;

namespace {

/// The publisher whose reference fixings the derived source reads.
constexpr std::string_view fixing_provider = "ecb";

/// The history read for a pair: the five years every environment holds, and a margin.
constexpr int window_days = 1830;

/// The days of one read of what a series holds: 120 days of 40 quotes stay under the ceiling
/// of one reply.
constexpr int stored_chunk_days = 120;

/// The observations one write carries, well under the broker's megabyte.
constexpr std::size_t write_batch = 1000;

/// An id read from text, or nothing for text that is not one.
std::optional<boost::uuids::uuid> uuid_of(const std::string& text) {
    try {
        return boost::uuids::string_generator{}(text);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}

derived_fx_vol_adapter::derived_fx_vol_adapter(ores::nats::service::client& nats,
                                               ores::nats::service::nats_client& svc_nats,
                                               ores::database::context db_ctx,
                                               std::string source_name,
                                               std::string provider_code,
                                               boost::uuids::uuid feed_id,
                                               derived_fx_vol_timing timing)
    : nats_(nats)
    , svc_nats_(svc_nats)
    , db_ctx_(std::move(db_ctx))
    , source_name_(std::move(source_name))
    , provider_code_(std::move(provider_code))
    , feed_id_(feed_id)
    , timing_(timing)
    // The readers and the store capture this and run only once the loop starts, after every
    // member they use is built; feed_ is declared after all of them.
    , feed_(source_name_, provider_code_, make_readers(), {}, make_store()) {}

derived_fx_vol_adapter::~derived_fx_vol_adapter() {
    stop();
}

feed::derived_fx_readers derived_fx_vol_adapter::make_readers() {
    return {.fixings = [this](const std::string& item, std::chrono::year_month_day to)
                -> std::expected<feed::dated_series, std::string> {
                const auto party = system_party();
                if (!party)
                    return std::unexpected(party.error());
                auto delegated = svc_nats_.with_delegation({});
                ores::marketdata::client::market_data_client md(delegated);
                const auto window =
                    md.get_fixing_window({.item = item,
                                          .provider_code = std::string(fixing_provider),
                                          .party_id = *party,
                                          .to = to,
                                          .days = window_days});
                if (!window)
                    return std::unexpected(window.error());
                feed::dated_series out;
                const auto n = std::min(window->dates.size(), window->values.size());
                out.dates.reserve(n);
                out.values.reserve(n);
                std::size_t unread = 0;
                for (std::size_t i = 0; i < n; ++i) {
                    const auto& text = window->values[i];
                    double value = 0.0;
                    const auto [end, ec] =
                        std::from_chars(text.data(), text.data() + text.size(), value);
                    if (ec != std::errc{} || end != text.data() + text.size()) {
                        ++unread;
                        continue;
                    }
                    out.dates.push_back(window->dates[i]);
                    out.values.push_back(value);
                }
                if (unread > 0)
                    BOOST_LOG_SEV(lg(), warn) << unread << " fixing(s) of " << item
                                              << " are not numbers and are left out";
                return out;
            },
            .convention = [this](const feed::derived_fx_pair& pair)
                -> std::expected<ores::refdata::domain::fx_option_convention, std::string> {
                const auto id = feed::fx_option_convention_id(pair);
                ores::refdata::repository::fx_option_convention_repository repo;
                const auto found = repo.read_latest(db_ctx_, id);
                if (found.empty())
                    return std::unexpected("no FX option convention '" + id + "'");
                return found.front();
            }};
}

std::expected<std::string, std::string> derived_fx_vol_adapter::system_party() {
    if (!system_party_) {
        ores::refdata::repository::party_repository parties;
        const auto system = parties.read_system_party(db_ctx_, db_ctx_.tenant_id().to_string());
        if (system.empty())
            return std::unexpected("the tenant has no system party");
        system_party_ = boost::uuids::to_string(system.front().id);
    }
    return *system_party_;
}

feed::derived_fx_store derived_fx_vol_adapter::make_store() {
    return {
        .ensure_series =
            [this](const feed::derived_fx_pair& pair) -> std::expected<std::string, std::string> {
            const auto party = system_party();
            if (!party)
                return std::unexpected(party.error());
            const auto party_id = uuid_of(*party);
            if (!party_id)
                return std::unexpected("the system party is not an id");
            const auto uri = feed::derived_fx_series_uri(pair);
            ores::marketdata::client::market_data_client md(svc_nats_);
            const auto started = std::chrono::steady_clock::now();
            const auto series = md.list_series_of_provider(provider_code_);
            BOOST_LOG_SEV(lg(), debug)
                << "Listed the series of provider " << provider_code_ << " in "
                << std::chrono::duration<double>(std::chrono::steady_clock::now() - started).count()
                << "s: " << (series ? std::to_string(series->size()) + " series" : "failed");
            if (!series)
                return std::unexpected(series.error());
            for (const auto& s : *series)
                if (s.oresmd_uri == uri && boost::uuids::to_string(s.party_id) == *party)
                    return boost::uuids::to_string(s.id);
            const auto written = md.put_series(
                {.id = boost::uuids::name_generator_sha1(feed::derived_fx_method_id())(uri + "|" +
                                                                                       *party),
                 .party_id = *party_id,
                 .oresmd_uri = uri,
                 .provider_code = provider_code_,
                 .series_subclass = "volatility",
                 .derivation_kind = std::string(feed::derived_fx_derivation_kind),
                 .derivation_config_id = feed::derived_fx_method_id(),
                 .derivation_config_version = feed::derived_fx_method_version},
                {.reason_code = "system.new_record",
                 .commentary = "The series the derived FX volatility quotes are kept under"});
            if (!written)
                return std::unexpected(written.error());
            return boost::uuids::to_string(written->id);
        },
        .stored_days = [this](const std::string&,
                              const feed::derived_fx_pair& pair,
                              std::chrono::year_month_day to)
            -> std::expected<std::set<std::chrono::year_month_day>, std::string> {
            const auto party = system_party();
            if (!party)
                return std::unexpected(party.error());
            auto delegated = svc_nats_.with_delegation({});
            ores::marketdata::client::market_data_client md(delegated);
            std::set<std::chrono::year_month_day> held;
            // A window holds the days from to - days + 1 to to, both included, so windows
            // ending stored_chunk_days apart tile the history with no gap and no overlap.
            for (int back = 0; back < window_days; back += stored_chunk_days) {
                const auto end = std::chrono::year_month_day{std::chrono::sys_days{to} -
                                                             std::chrono::days{back}};
                const auto started = std::chrono::steady_clock::now();
                const auto window =
                    md.get_observation_window({.item = feed::derived_fx_series_uri(pair),
                                               .provider_code = provider_code_,
                                               .party_id = *party,
                                               .to = end,
                                               .days = stored_chunk_days});
                BOOST_LOG_SEV(lg(), debug)
                    << "Read the stored days of " << feed::derived_fx_series_uri(pair) << " ending "
                    << end << " in "
                    << std::chrono::duration<double>(std::chrono::steady_clock::now() - started)
                           .count()
                    << "s: "
                    << (window ? std::to_string(window->observed_at.size()) + " day(s)" :
                                 "failed, " + window.error());
                if (!window)
                    return std::unexpected(window.error());
                for (const auto& at : window->observed_at)
                    held.insert(
                        std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(at)});
            }
            return held;
        },
        .write = [this](const std::string& series_id, const std::vector<feed::derived_fx_day>& days)
            -> std::expected<void, std::string> {
            const auto party = system_party();
            if (!party)
                return std::unexpected(party.error());
            const auto party_id = uuid_of(*party);
            const auto series = uuid_of(series_id);
            if (!party_id || !series)
                return std::unexpected("the party or the series is not an id");
            // Writes go as the service itself, which holds the write grants; reads go
            // delegated, as every read of the source does.
            ores::marketdata::client::market_data_client md(svc_nats_);
            boost::uuids::random_generator ids;
            std::vector<mm::market_observation_write> batch;
            const auto flush = [&]() -> std::expected<void, std::string> {
                if (batch.empty())
                    return {};
                const auto started = std::chrono::steady_clock::now();
                const auto stored =
                    md.put_observations(batch,
                                        {.reason_code = "system.new_record",
                                         .commentary = "Derived FX volatility quotes"});
                BOOST_LOG_SEV(lg(), debug)
                    << "Wrote " << batch.size() << " derived observation(s) of series " << series_id
                    << " in "
                    << std::chrono::duration<double>(std::chrono::steady_clock::now() - started)
                           .count()
                    << "s" << (stored ? "" : ": failed");
                batch.clear();
                if (!stored)
                    return std::unexpected(stored.error());
                return {};
            };
            // A batch ends only between days, so a write that fails leaves no day held in
            // part, which the days held would then count as whole.
            for (const auto& day : days) {
                if (batch.size() + day.points.size() > write_batch)
                    if (const auto r = flush(); !r)
                        return r;
                for (const auto& point : day.points)
                    batch.push_back({.id = ids(),
                                     .party_id = *party_id,
                                     .series_id = *series,
                                     .observation_datetime =
                                         std::chrono::sys_days{day.day} + std::chrono::hours(17),
                                     .oresmd_uri = point.coordinate,
                                     .value = std::format("{}", point.value),
                                     .source = provider_code_,
                                     .feed_id = feed_id_});
            }
            return flush();
        }};
}

void derived_fx_vol_adapter::start() {
    const auto subject = subjects::adapter_demand_subject(source_name_);
    BOOST_LOG_SEV(lg(), info) << "Starting the derived source '" << source_name_ << "': demand on '"
                              << subject << "'";
    demand_sub_ =
        nats_.subscribe(subject, [this](ores::nats::message msg) { demand_message(msg); });
    thread_ = std::thread(&derived_fx_vol_adapter::loop, this);
    heartbeat_thread_ = std::thread(&derived_fx_vol_adapter::heartbeat_loop, this);
}

void derived_fx_vol_adapter::stop() {
    stop_.store(true, std::memory_order_relaxed);
    demand_sub_.reset();
    if (thread_.joinable())
        thread_.join();
    if (heartbeat_thread_.joinable())
        heartbeat_thread_.join();
}

bool derived_fx_vol_adapter::load_universe() {
    try {
        ores::marketdata::client::market_data_client md(svc_nats_);
        const auto instruments = md.list_vendor_instruments(provider_code_);
        if (!instruments) {
            BOOST_LOG_SEV(lg(), warn)
                << "Cannot read the universe of '" << source_name_ << "': " << instruments.error();
            return false;
        }
        std::vector<std::string> ids;
        ids.reserve(instruments->size());
        for (const auto& v : *instruments)
            if (v.id_scheme == feed::derived_fx_id_scheme && v.field == feed::derived_fx_field)
                ids.push_back(v.external_id);
        feed_.set_universe(ids);
        return feed_.has_universe();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot read the universe of '" << source_name_
                                  << "': " << e.what();
        return false;
    }
}

void derived_fx_vol_adapter::demand_message(const ores::nats::message& msg) {
    auto request = ores::nats::default_wire_codec().decode<mm::adapter_demand_request>(msg.data);
    if (!request) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to decode a demand on '" << msg.subject
                                  << "': " << request.error().what();
        return;
    }
    // Until the universe is read the source has not announced, and it refuses every id as
    // not ready; the service sends the demand again once it announces. Once ready, the
    // universe no longer changes, so the answer reads it while the loop refreshes.
    mm::adapter_demand_response response;
    if (!ready_.load(std::memory_order_acquire)) {
        for (const auto& id : request->ids)
            response.rejected.push_back({id, "not_ready"});
    } else {
        response = feed_.answer(*request);
        std::lock_guard lock(mu_);
        pending_ = pending_ ? feed::coalesce_demands(std::move(*pending_), std::move(*request)) :
                              std::move(*request);
    }
    ores::service::messaging::reply(nats_, msg, response);
}

bool derived_fx_vol_adapter::announce() {
    try {
        auto request = feed_.announcement();
        request.adapter_code = "derived";
        request.module_version = ORES_VERSION;
        const auto bytes = ores::nats::default_wire_codec().encode(request);
        const auto reply = nats_.request_sync(subjects::adapter_announce_subject(),
                                              std::span<const std::byte>(bytes),
                                              {},
                                              timing_.reply_timeout);
        const auto response =
            ores::nats::default_wire_codec().decode<mm::adapter_announce_response>(reply.data);
        if (!response || !response->success) {
            BOOST_LOG_SEV(lg(), warn)
                << "The service refused the announce of '" << source_name_
                << "': " << (response ? response->message : response.error().what());
            return false;
        }
        BOOST_LOG_SEV(lg(), info) << "Announced '" << source_name_
                                  << "': " << response->served_count << " served, "
                                  << response->unknown_count << " unknown";
        return true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), debug)
            << "The announce of '" << source_name_ << "' got no answer: " << e.what();
        return false;
    }
}

void derived_fx_vol_adapter::serve(clock::time_point now) {
    if (!ready_.load(std::memory_order_relaxed)) {
        if (now < next_universe_)
            return;
        if (!load_universe()) {
            next_universe_ = now + timing_.retry_interval;
            return;
        }
        ready_.store(true, std::memory_order_release);
    }
    if (now >= next_announce_) {
        const bool announced = announce();
        announced_.store(announced, std::memory_order_release);
        next_announce_ = now + (announced ? timing_.reannounce_interval : timing_.retry_interval);
    }
    if (!announced_.load(std::memory_order_acquire))
        return;

    std::optional<mm::adapter_demand_request> taken;
    {
        std::lock_guard lock(mu_);
        taken.swap(pending_);
    }
    if (taken) {
        feed_.demand(*taken);
        next_refresh_ = now;
    }

    const auto today = std::chrono::year_month_day{
        std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now())};
    // A publish that fails, such as on a lost connection, ends this refresh: the pair it
    // was sending is sent again on the next, since its day is recorded only once sent.
    // The latest quotes go out before the history is kept: a party's series of a derived
    // quote exists only once a quote reaches it, and a fresh history takes many minutes
    // to write, during which provisioning waits for those series.
    if (now >= next_refresh_) {
        try {
            const auto sent = feed_.refresh(today, [&](const mm::adapter_tick& tick) {
                nats_.publish(subjects::adapter_ingress_subject(source_name_, tick.external_id),
                              ores::nats::default_wire_codec().encode(tick));
            });
            for (const auto& failure : feed_.failures())
                BOOST_LOG_SEV(lg(), warn) << "The derived source left out " << failure;
            if (sent > 0)
                BOOST_LOG_SEV(lg(), info)
                    << "Sent " << sent << " derived quote(s) of '" << source_name_ << "'";
            history_pending_ = true;
            next_refresh_ = now + timing_.refresh_interval;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to refresh '" << source_name_ << "': " << e.what();
            next_refresh_ = now + timing_.retry_interval;
            return;
        }
    }
    if (!history_pending_ || now < next_history_)
        return;
    // One pair a pass, so a demand that arrives while the history is written, such as a
    // party subscribing during provisioning, is answered on the next pass rather than
    // after every pair.
    try {
        const auto kept = feed_.persist(today, 1);
        for (const auto& failure : feed_.failures())
            BOOST_LOG_SEV(lg(), warn) << "The derived history left out " << failure;
        if (kept > 0)
            BOOST_LOG_SEV(lg(), info)
                << "Kept " << kept << " day(s) of derived quotes of '" << source_name_ << "'";
        history_pending_ = kept > 0;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to keep the history of '" << source_name_
                                  << "': " << e.what();
        next_history_ = now + timing_.retry_interval;
    }
}

void derived_fx_vol_adapter::heartbeat_loop() {
    constexpr auto slice = std::chrono::milliseconds(50);
    auto next = clock::now();
    while (!stop_.load(std::memory_order_relaxed)) {
        const auto now = clock::now();
        if (now >= next && announced_.load(std::memory_order_acquire)) {
            try {
                mm::adapter_heartbeat beat;
                beat.adapter_id = source_name_;
                beat.counter = ++beats_;
                nats_.publish(subjects::adapter_heartbeat_subject(source_name_),
                              ores::nats::default_wire_codec().encode(beat));
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Failed to send the heartbeat of '" << source_name_ << "': " << e.what();
            }
            next = now + timing_.heartbeat_interval;
        }
        std::this_thread::sleep_for(slice);
    }
}

void derived_fx_vol_adapter::loop() {
    constexpr auto slice = std::chrono::milliseconds(50);
    while (!stop_.load(std::memory_order_relaxed)) {
        serve(clock::now());
        std::this_thread::sleep_for(slice);
    }
}

}
