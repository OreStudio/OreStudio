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
#ifndef ORES_SERVICE_SERVICE_RECONCILER_HPP
#define ORES_SERVICE_SERVICE_RECONCILER_HPP

#include "ores.service/export.hpp"
#include <chrono>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <utility>

namespace ores::service::service {

/**
 * @brief What one pass of a reconciler left.
 *
 * A pass that leaves nothing is done. A pass that leaves work states why, so
 * the reconciler can log the reason once and run the pass again later.
 */
struct reconcile_result {
    bool done = true;
    std::string pending_reason;

    [[nodiscard]] static reconcile_result finished() {
        return {};
    }
    [[nodiscard]] static reconcile_result pending(std::string reason) {
        return {.done = false, .pending_reason = std::move(reason)};
    }
};

/**
 * @brief How a reconciler waits between passes.
 */
struct reconciler_options {
    /// The name the reconciler logs under, such as "fixing history".
    std::string name;
    /// The wait after the first pass that leaves work.
    std::chrono::milliseconds first_delay{std::chrono::seconds(1)};
    /// The longest wait between two passes that leave work.
    std::chrono::milliseconds max_delay{std::chrono::seconds(60)};
    /// When set, a pass also runs this long after the last one that was done.
    std::optional<std::chrono::milliseconds> interval;
};

/**
 * @brief The wait after a pass that left work, given the wait before it.
 *
 * The first wait is @p first; each later one doubles, and none passes @p max.
 */
[[nodiscard]] ORES_SERVICE_EXPORT std::chrono::milliseconds
next_reconcile_delay(std::optional<std::chrono::milliseconds> previous,
                     std::chrono::milliseconds first,
                     std::chrono::milliseconds max);

/**
 * @brief Runs a pass until it leaves no work, on a thread of its own.
 *
 * A reconciler holds a service's work that calls another service: the pass
 * reads what exists, does what is missing, and is safe to run again. A pass
 * that leaves work, because the service it calls does not answer yet or has
 * gone, runs again after a wait that doubles to a cap. kick() asks for a pass
 * now, which is what new work does. So the work completes once the other
 * service answers, whatever order the two started in, and a service whose
 * dependency comes back needs no restart.
 *
 * The pass runs on the reconciler's thread, never two at once. It may block,
 * so it does not run on a service's io_context.
 */
class ORES_SERVICE_EXPORT reconciler final {
public:
    using pass_fn = std::function<reconcile_result()>;

    reconciler(reconciler_options options, pass_fn pass);
    ~reconciler();

    reconciler(const reconciler&) = delete;
    reconciler& operator=(const reconciler&) = delete;

    /// Asks for a pass now. A pass that is running finishes first.
    void kick();

    /// Stops the thread after the pass that is running, if any.
    void stop();

private:
    void run();

    const reconciler_options options_;
    const pass_fn pass_;
    std::mutex mu_;
    std::condition_variable wake_;
    bool kicked_ = false;
    bool stopping_ = false;
    std::thread thread_;
};

}

#endif
