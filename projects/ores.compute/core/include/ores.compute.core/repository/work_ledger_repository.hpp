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
#ifndef ORES_COMPUTE_REPOSITORY_WORK_LEDGER_REPOSITORY_HPP
#define ORES_COMPUTE_REPOSITORY_WORK_LEDGER_REPOSITORY_HPP

#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::repository {

/**
 * @brief A result that waits in the queue, as the dispatcher records it.
 */
struct ledger_entry {
    boost::uuids::uuid result_id;
    boost::uuids::uuid work_tenant_id;
    boost::uuids::uuid workunit_id;
    std::vector<std::string> platforms;
    int priority = 0;
};

/**
 * @brief The job a claim took, with what is needed to find its result.
 */
struct claimed_entry {
    boost::uuids::uuid result_id;
    boost::uuids::uuid work_tenant_id;
    boost::uuids::uuid workunit_id;
    int attempts = 0;
};

/**
 * @brief The number of times a job is claimed before it ends as no reply.
 */
inline constexpr int max_claim_attempts = 3;

/**
 * @brief A claimed entry whose lease ended, with the host that held it.
 */
struct expired_entry {
    boost::uuids::uuid result_id;
    boost::uuids::uuid work_tenant_id;
    boost::uuids::uuid host_id;
    int attempts = 0;
};

/**
 * @brief The queue of the compute grid, held in the work ledger.
 *
 * The ledger belongs to the installation. Only a session of the system tenant
 * reads or writes it, and the policy decides that, so every method here runs
 * with the service's own context and states no tenant predicate. A tenant's
 * result is written elsewhere, by entering the tenant.
 *
 * Every statement that changes an entry is atomic, so concurrent claims, and a
 * claim against a heartbeat or a reaper, never act on one entry twice.
 */
class ORES_COMPUTE_CORE_EXPORT work_ledger_repository {
private:
    inline static std::string_view logger_name = "ores.compute.repository.work_ledger_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Queue a result. A result that is already in the ledger is left as it is.
     */
    void enqueue(context ctx, const ledger_entry& entry);

    /**
     * @brief Take the next waiting entry that a node of a platform can run.
     *
     * One statement picks the entry and marks it claimed for the host, sets its
     * lease and adds one to its attempts. It skips an entry that another claim
     * holds. Among the waiting entries it prefers the tenant with the fewest
     * entries claimed, so a tenant with a deep queue cannot hold the others back,
     * and then the higher priority and then the older entry.
     *
     * @return The entry, or nothing when none waits for the platform.
     */
    std::optional<claimed_entry> claim(context ctx,
                                       const boost::uuids::uuid& host_id,
                                       const std::string& platform,
                                       std::chrono::seconds lease);

    /**
     * @brief Give a claimed entry back to the queue, as if it had not been taken.
     *
     * Used when a claim cannot be completed after the entry was taken, for
     * example when no storage token could be minted. The attempt the claim
     * added is given back, because no node ran the job, so an outage of a
     * service the claim needs never counts against the job's attempts.
     */
    void release(context ctx, const boost::uuids::uuid& result_id);

    /**
     * @brief Remove an entry: its result is done, retired or ended.
     */
    void remove(context ctx, const boost::uuids::uuid& result_id);

    /**
     * @brief Extend the lease of the one entry a host is working on.
     *
     * A host runs one job at a time and names it in its heartbeat. An entry the host
     * holds and does not name is not being worked on, so its lease is left to run out.
     * Extending every entry of a host would keep an abandoned claim alive for as long as
     * the host lived.
     *
     * @return True when the host holds the entry and it was extended.
     */
    bool extend_lease(context ctx,
                      const boost::uuids::uuid& host_id,
                      const boost::uuids::uuid& result_id,
                      std::chrono::seconds lease);

    /**
     * @brief Return to the queue every entry a host holds, keeping their attempts.
     *
     * A host that asks for work is free, because it runs one job at a time. An entry it still
     * holds was claimed and never reported, for example when the reply to the claim was lost,
     * so nobody is working on it. An entry with no attempts left is left for the reaper.
     *
     * @return The number of entries returned.
     */
    std::size_t requeue_held(context ctx,
                             const boost::uuids::uuid& host_id,
                             int max_attempts = max_claim_attempts);

    /**
     * @brief Return every claimed entry whose lease ended to the queue, while it has attempts left.
     *
     * One statement, so a reaper and a claim never act on one entry twice. The entry keeps its
     * queue time, so it goes back to its place and not to the end of the line.
     *
     * @return The number of entries returned.
     */
    std::size_t requeue_expired(context ctx, int max_attempts = max_claim_attempts);

    /**
     * @brief The claimed entries whose lease ended and that have no attempts left.
     *
     * Reads only. The reaper ends the result of each and then calls @c remove_expired.
     */
    std::vector<expired_entry> exhausted(context ctx,
                                         int max_attempts = max_claim_attempts,
                                         int limit = 100);

    /**
     * @brief Remove an entry only if its lease has still ended.
     *
     * A host that renewed the lease after the reaper read the entry keeps it.
     *
     * @return True when the entry was removed.
     */
    bool remove_expired(context ctx, const boost::uuids::uuid& result_id);

    /**
     * @brief The number of entries of a tenant that wait in the queue.
     */
    std::int64_t waiting(context ctx, const boost::uuids::uuid& work_tenant_id);

    /**
     * @brief The host that holds the claim on a result, when one does.
     *
     * Nothing is returned for a result with no entry and for an entry that waits.
     * A result with no entry was handed out by the push queue.
     */
    std::optional<boost::uuids::uuid> holder(context ctx, const boost::uuids::uuid& result_id);

    /**
     * @brief The number of entries in the ledger in a state, for a test and a screen.
     */
    std::int64_t count(context ctx, const std::string& state);
};

}

#endif
