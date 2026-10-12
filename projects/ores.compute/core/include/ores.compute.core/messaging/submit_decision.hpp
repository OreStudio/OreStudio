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
#ifndef ORES_COMPUTE_CORE_MESSAGING_SUBMIT_DECISION_HPP
#define ORES_COMPUTE_CORE_MESSAGING_SUBMIT_DECISION_HPP

#include <boost/uuid/uuid.hpp>
#include <optional>

namespace ores::compute::messaging {

/**
 * @brief What a submitted outcome does to the result it names.
 */
enum class submit_action {
    /// The result is waiting or running, so the outcome is recorded.
    accept,
    /// A person or a process retired the result, so the outcome is not recorded.
    ignore_inactive,
    /// The result already has its outcome, so a second submit changes nothing.
    ignore_final,
};

/**
 * @brief Decides what a submit does from the server state of the result.
 *
 * The states are 1 inactive, 2 unsent, 4 in progress and 5 done. A queue can
 * deliver an assignment more than once, and a node can take one whose result
 * was retired while it waited. Recording an outcome for either would rewrite
 * a settled result: a retired one would come back as a failure, and a done one
 * would lose its outcome. Both are ignored, and the node is told it succeeded
 * so it never reports an error for an assignment that cannot matter.
 */
constexpr submit_action decide_submit(int server_state) {
    if (server_state == 1)
        return submit_action::ignore_inactive;
    if (server_state == 5)
        return submit_action::ignore_final;
    return submit_action::accept;
}

/**
 * @brief Whether a claim can hand a result to a node, from its server state and the attempt.
 *
 * An unsent result can always run. A result in progress can run only on a later attempt,
 * because the reaper returned the entry to the queue after the lease of the earlier node
 * ended, and left the result as that node marked it. A first attempt on a result in
 * progress is an entry that points at a result another claim already took.
 */
constexpr bool claimable(int server_state, int attempt) {
    return server_state == 2 || (server_state == 4 && attempt > 1);
}

/**
 * @brief Whether a result's claim belongs to a host other than the one that submits.
 *
 * The work ledger names the host that holds the claim. A result whose lease ended
 * goes to another host, and the first host may still finish and submit. Its
 * outcome must not overwrite the result of the host that took over, so a submit
 * from a host that is not the holder is ignored. A result with no holder was
 * handed out by the push queue, or is not claimed, and any host may submit it.
 */
inline bool claimed_by_another(const std::optional<boost::uuids::uuid>& holder,
                               const std::optional<boost::uuids::uuid>& submitter) {
    return holder.has_value() && (!submitter.has_value() || *submitter != *holder);
}

} // namespace ores::compute::messaging

#endif
