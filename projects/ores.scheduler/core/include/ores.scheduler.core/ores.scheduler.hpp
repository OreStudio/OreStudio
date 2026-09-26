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
#ifndef ORES_SCHEDULER_HPP
#define ORES_SCHEDULER_HPP

/**
 * @brief Job scheduling for ORE Studio.
 *
 * The component fires recurring jobs from cron expressions. A job definition
 * is persisted configuration: a name, a schedule, an action and an active
 * flag. The scheduler loop loads every active definition across all tenants
 * and fires each one, recording a job instance per execution. Two action
 * types exist: run SQL, or publish a NATS message.
 *
 * The job-definition stack -- domain type, protocol, repository, service,
 * handler, registrars, history provider, generator and SQL schema -- is
 * generated from the entity model at
 * projects/ores.scheduler/modeling/ores.scheduler.job_definition.org. The
 * hand-written code is the infrastructure generation does not reach: the
 * scheduler loop, the cron evaluator, the action handlers, the messaging
 * registrar, the service application, and the two computed views -- the job
 * instance list and the live scheduler status -- which are declared by the
 * operation model and served by hand-written handlers.
 *
 * A job may belong to a tenant or to none. System jobs, such as the MQ
 * statistics scrape, carry a NULL tenant, which is why the tenant column is
 * nullable and why the scheduler loop reads across tenants rather than
 * through the tenant-scoped read set.
 */
namespace ores::scheduler {}

#endif
