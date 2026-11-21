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
#include "ores.service/service/reconciler.hpp"
#include "ores.logging/logger.hpp"
#include <algorithm>
#include <exception>
#include <utility>

namespace ores::service::service {

namespace {

auto& reconciler_lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.service.service.reconciler");
    return instance;
}

}

std::chrono::milliseconds next_reconcile_delay(std::optional<std::chrono::milliseconds> previous,
                                               std::chrono::milliseconds first,
                                               std::chrono::milliseconds max) {
    if (!previous)
        return std::min(first, max);
    return std::min(*previous * 2, max);
}

reconciler::reconciler(reconciler_options options, pass_fn pass)
    : options_(std::move(options))
    , pass_(std::move(pass))
    , thread_([this] { run(); }) {}

reconciler::~reconciler() {
    stop();
}

void reconciler::kick() {
    {
        std::lock_guard lock(mu_);
        kicked_ = true;
    }
    wake_.notify_one();
}

void reconciler::stop() {
    {
        std::lock_guard lock(mu_);
        stopping_ = true;
    }
    wake_.notify_all();
    if (thread_.joinable())
        thread_.join();
}

void reconciler::run() {
    using namespace ores::logging;
    std::optional<std::chrono::milliseconds> delay;
    std::string last_reason;
    // A pass runs on a kick, which new work brings, or when a wait times out:
    // the interval after a pass that was done, the backoff after one that was
    // not. Without an interval, a reconciler that is done waits for a kick.
    std::optional<std::chrono::milliseconds> wait = options_.interval;
    while (true) {
        {
            std::unique_lock lock(mu_);
            const auto woken = [this] {
                return stopping_ || kicked_;
            };
            if (wait)
                wake_.wait_for(lock, *wait, woken);
            else
                wake_.wait(lock, woken);
            if (stopping_)
                return;
            kicked_ = false;
        }

        reconcile_result result;
        try {
            result = pass_();
        } catch (const std::exception& e) {
            result = reconcile_result::pending(e.what());
        }

        if (result.done) {
            if (delay)
                ORES_LOG_SEV(reconciler_lg(), info)
                    << "The " << options_.name << " reconciler is done.";
            delay.reset();
            last_reason.clear();
            wait = options_.interval;
            continue;
        }

        delay = next_reconcile_delay(delay, options_.first_delay, options_.max_delay);
        // The same reason on every pass would fill the log while a dependency
        // is away, so a reason is logged as a warning only when it changes.
        if (result.pending_reason != last_reason) {
            ORES_LOG_SEV(reconciler_lg(), warn)
                << "The " << options_.name << " reconciler left work: " << result.pending_reason
                << ". It runs again in " << delay->count() << " ms.";
            last_reason = result.pending_reason;
        } else {
            ORES_LOG_SEV(reconciler_lg(), debug)
                << "The " << options_.name << " reconciler still has work; it runs again in "
                << delay->count() << " ms.";
        }
        // The wait ends at its timeout with no kick, so the pass runs again.
        wait = delay;
    }
}

}
