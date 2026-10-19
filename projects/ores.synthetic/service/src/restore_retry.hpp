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
#ifndef ORES_SYNTHETIC_SERVICE_RESTORE_RETRY_HPP
#define ORES_SYNTHETIC_SERVICE_RESTORE_RETRY_HPP

#include <algorithm>
#include <chrono>
#include <cstddef>

namespace ores::synthetic::service {

/**
 * @brief How long a restore waits for the services it depends on.
 *
 * The services of an installation start in no fixed order, so a feed that
 * needs marketdata or IAM can find neither one at the first pass. The waits
 * start at @c first_wait and double to @c longest_wait. The restore stops when
 * the waits of one run would pass @c budget.
 */
struct restore_retry_policy final {
    std::chrono::milliseconds first_wait{2000};
    std::chrono::milliseconds longest_wait{30000};
    std::chrono::milliseconds budget{300000};
};

/**
 * @brief Runs @p pass until it leaves nothing to do, or the policy is spent.
 *
 * The caller has made the first pass and passes the number of feeds that it
 * left in @p remaining. @p pass makes one try for those feeds and answers how
 * many remain. @p sleep waits for the span it is given. @p stopped answers
 * whether the service is shutting down, and it is read before each wait.
 *
 * @return The number of feeds that remain when the loop ends.
 */
template <typename Pass, typename Sleep, typename Stopped>
std::size_t retry_restore(const restore_retry_policy& policy,
                          std::size_t remaining,
                          Pass&& pass,
                          Sleep&& sleep,
                          Stopped&& stopped) {
    auto wait = policy.first_wait;
    std::chrono::milliseconds spent{0};
    while (remaining > 0 && !stopped() && spent + wait <= policy.budget) {
        sleep(wait);
        spent += wait;
        remaining = static_cast<std::size_t>(pass());
        wait = std::min(wait * 2, policy.longest_wait);
    }
    return remaining;
}

}

#endif
