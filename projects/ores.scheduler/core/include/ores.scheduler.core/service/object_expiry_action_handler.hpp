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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SCHEDULER_CORE_SERVICE_OBJECT_EXPIRY_ACTION_HANDLER_HPP
#define ORES_SCHEDULER_CORE_SERVICE_OBJECT_EXPIRY_ACTION_HANDLER_HPP

#include "ores.nats/service/nats_client.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/service/action_handler.hpp"

namespace ores::scheduler::service {

/**
 * @brief Expires the object prefixes whose policies are due.
 *
 * Handles jobs with action_type == "expire_prefix". The sweep that bounds the
 * tables is a SQL function and cannot reach an object store, so the object half
 * of housekeeping is an action of its own: it asks the database which object
 * policies are due, and asks the storage service to expire each one.
 *
 * The database answers both halves of the bookkeeping. Which policies are due is
 * decided by =ores_scheduler_due_object_policies_fn=, beside the rule the table
 * sweep uses, so the two cannot disagree about when a store is next treated; and
 * what each run did is written by =ores_scheduler_record_object_run_fn=, so the
 * ledger's columns are written in one place.
 *
 * The payload may state =dry_run=: the same path then records what the cutoff
 * held and removes nothing, which is how a person reads the plan before a sweep
 * acts on it.
 */
class ORES_SCHEDULER_CORE_EXPORT object_expiry_action_handler final : public action_handler {
public:
    explicit object_expiry_action_handler(ores::nats::service::nats_client& svc_nats);

    [[nodiscard]] std::string_view action_type() const noexcept override {
        return "expire_prefix";
    }

    boost::asio::awaitable<std::expected<void, std::string>>
    execute(const action_context& ctx) override;

private:
    ores::nats::service::nats_client& svc_nats_;
};

}

#endif
