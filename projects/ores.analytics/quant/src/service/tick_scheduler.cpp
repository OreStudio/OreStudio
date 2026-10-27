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
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include <cmath>
#include <stdexcept>
#include <utility>

namespace ores::analytics::quant::service {

tick_scheduler::tick_scheduler(i_run_clock& clock,
                               const domain::market_config& market,
                               std::uint64_t run_seed)
    : clock_(clock)
    , origin_(clock.now())
    , run_seed_(run_seed)
    , market_(market,
              math::rng_stream(math::stream_key{.run_seed = run_seed,
                                                .series = math::series_key("market"),
                                                .purpose = math::draw_purpose::market})) {}

double tick_scheduler::seconds_at(i_run_clock::time_point t) const {
    return std::chrono::duration<double>(t - origin_).count();
}

i_run_clock::time_point tick_scheduler::time_at(double seconds) const {
    return origin_ + std::chrono::duration_cast<i_run_clock::time_point::duration>(
                         std::chrono::duration<double>(seconds));
}

void tick_scheduler::advance_to(double seconds) {
    const double by = seconds - market_.now();
    if (by > 0.0)
        market_.advance(by);
}

void tick_scheduler::post(command c, bool wait) {
    std::unique_lock lock(mutex_);
    if (!running_ || std::this_thread::get_id() == loop_thread_) {
        advance_to(seconds_at(clock_.now()));
        c();
        return;
    }
    commands_.push_back(std::move(c));
    const auto ticket = ++posted_count_;
    lock.unlock();
    clock_.interrupt();
    if (!wait)
        return;
    lock.lock();
    applied_.wait(lock, [&] { return applied_count_ >= ticket || !running_; });
}

void tick_scheduler::drain(std::unique_lock<std::mutex>&) {
    auto taken = std::exchange(commands_, {});
    for (auto& c : taken)
        c();
    applied_count_ += taken.size();
    applied_.notify_all();
}

void tick_scheduler::schedule(std::size_t id, series& s) {
    s.gap = domain::draw_gap(s.law, s.stream);
    s.mark = market_.tau() + s.gap / s.spec.sensitivity;
    heap_.push(pending{.mark = s.mark, .id = id});
}

std::size_t tick_scheduler::add(const series_spec& spec, stream_body body) {
    if (!std::isfinite(spec.sensitivity) || spec.sensitivity <= 0.0)
        throw std::invalid_argument("tick_scheduler: sensitivity must be finite and positive");
    math::rng_stream stream(math::stream_key{
        .run_seed = run_seed_, .series = spec.series, .purpose = math::draw_purpose::arrival});
    auto law = domain::make_arrival_law(spec.arrival, stream);

    std::size_t id = 0;
    {
        const auto guard = std::lock_guard(mutex_);
        id = next_id_++;
    }
    post(
        [this, id, spec, law = std::move(law), stream, body = std::move(body)] {
            market_.enrol(spec.arrival.ticks_per_hour / 3600.0);
            auto& s = series_
                          .emplace(id,
                                   series{.spec = spec,
                                          .law = law,
                                          .stream = stream,
                                          .body = body,
                                          .gap = 0.0,
                                          .mark = 0.0,
                                          .last_fired = market_.now()})
                          .first->second;
            schedule(id, s);
        },
        false);
    return id;
}

void tick_scheduler::remove(std::size_t id) {
    post(
        [this, id] {
            const auto it = series_.find(id);
            if (it == series_.end())
                return;
            market_.withdraw(it->second.spec.arrival.ticks_per_hour / 3600.0);
            series_.erase(it);
        },
        true);
}

void tick_scheduler::pin(std::optional<domain::market_state> state) {
    post(
        [this, state] {
            if (state)
                market_.pin(*state);
            else
                market_.unpin();
        },
        true);
}

market_snapshot tick_scheduler::snapshot() const {
    const auto guard = std::lock_guard(mutex_);
    const auto state = market_.state();
    return market_snapshot{.state = state,
                           .multiplier = market_.multiplier(state),
                           .pinned = market_.is_pinned(),
                           .heat = market_.heat(),
                           .pace = market_.pace(),
                           .tau = market_.tau(),
                           .jump = market_.jump(),
                           .decay = market_.decay(),
                           .loop_gain = market_.loop_gain(),
                           .population_rate = market_.population_rate()};
}

std::vector<heat_reading>
tick_scheduler::readings_since(std::chrono::system_clock::time_point from) const {
    const auto guard = std::lock_guard(mutex_);
    std::vector<heat_reading> taken;
    for (const auto& r : history_)
        if (r.at >= from)
            taken.push_back(r);
    return taken;
}

std::size_t tick_scheduler::size() const {
    const auto guard = std::lock_guard(mutex_);
    return series_.size();
}

void tick_scheduler::sample(i_run_clock::time_point t) {
    const auto at = clock_.wall(t);
    if (!history_.empty() && at - history_.back().at < sample_period)
        return;
    history_.push_back(heat_reading{.at = at, .heat = market_.heat()});
    while (history_.size() > history_capacity)
        history_.pop_front();
}

std::optional<double> tick_scheduler::next_due() {
    // A removed series leaves its mark behind; it is dropped when it surfaces.
    while (!heap_.empty()) {
        const auto top = heap_.top();
        const auto it = series_.find(top.id);
        if (it != series_.end() && it->second.mark == top.mark)
            return market_.now() + market_.time_to_reach(top.mark);
        heap_.pop();
    }
    return std::nullopt;
}

std::optional<fired_tick> tick_scheduler::fire(std::unique_lock<std::mutex>& lock) {
    const auto due = next_due();
    if (!due)
        return std::nullopt;
    advance_to(*due);
    const auto id = heap_.top().id;
    heap_.pop();
    auto& s = series_.at(id);

    market_.record_event();
    domain::record_event(s.law, s.gap);
    const tick_context context{.elapsed_seconds = *due - s.last_fired,
                               .observed_at = clock_.wall(time_at(*due))};
    s.last_fired = *due;
    const fired_tick fired{.id = id, .at = *due, .tau = market_.tau()};
    auto body = s.body;
    auto on_published = on_published_;
    auto on_failed = on_failed_;
    // Scheduled before the body runs, so a body that removes its own series
    // leaves nothing pointing at it.
    schedule(id, s);

    lock.unlock();
    try {
        const auto summary = body(context);
        const auto n = published_.fetch_add(1, std::memory_order_relaxed) + 1;
        if (on_published)
            on_published(n, summary);
    } catch (const std::exception& ex) {
        if (on_failed)
            on_failed(ex);
    }
    lock.lock();
    return fired;
}

std::optional<fired_tick> tick_scheduler::step() {
    std::unique_lock lock(mutex_);
    return fire(lock);
}

void tick_scheduler::run() {
    {
        const auto guard = std::lock_guard(mutex_);
        running_ = true;
        loop_thread_ = std::this_thread::get_id();
    }
    const double sample_seconds = std::chrono::duration<double>(sample_period).count();

    while (!stop_flag_.load(std::memory_order_relaxed)) {
        std::unique_lock lock(mutex_);
        const double now = seconds_at(clock_.now());

        // Ticks due by now go first and in order, so a command never makes one
        // late; then the market moves to now and the commands apply there.
        for (auto due = next_due(); due && *due <= now; due = next_due()) {
            fire(lock);
            if (stop_flag_.load(std::memory_order_relaxed))
                break;
        }
        advance_to(now);
        drain(lock);
        sample(time_at(now));

        const auto due = next_due();
        const double wake = due ? std::min(*due, now + sample_seconds) : now + sample_seconds;
        lock.unlock();
        clock_.wait_until(time_at(wake));
    }

    std::unique_lock lock(mutex_);
    drain(lock);
    running_ = false;
    applied_.notify_all();
}

void tick_scheduler::stop() {
    stop_flag_.store(true, std::memory_order_relaxed);
    clock_.interrupt();
}

void tick_scheduler::set_hooks(published_hook on_published, failure_hook on_failed) {
    const auto guard = std::lock_guard(mutex_);
    on_published_ = std::move(on_published);
    on_failed_ = std::move(on_failed);
}

}
