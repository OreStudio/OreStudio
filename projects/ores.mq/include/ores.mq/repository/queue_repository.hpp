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
#ifndef ORES_MQ_REPOSITORY_QUEUE_REPOSITORY_HPP
#define ORES_MQ_REPOSITORY_QUEUE_REPOSITORY_HPP

#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.mq/domain/queue_definition.hpp"

namespace ores::mq::repository {

/**
 * @brief Data access layer for queue definitions.
 *
 * Provides CRUD operations against ores_mq_queues_tbl via parameterized SQL.
 * All methods take an ores::database::context and are stateless.
 */
class queue_repository final {
public:
    using context = ores::database::context;

    /**
     * @brief Creates a new queue definition.
     *
     * Calls ores_mq_queues_create_fn and returns the assigned queue UUID.
     */
    boost::uuids::uuid create_queue(context ctx,
        const domain::queue_definition& def, const std::string& modified_by);

    /**
     * @brief Finds a queue definition by its UUID.
     *
     * @return The queue definition, or nullopt if not found.
     */
    std::optional<domain::queue_definition> find_by_id(context ctx,
        const boost::uuids::uuid& id);

    /**
     * @brief Finds a queue definition by name and optional tenant/party scope.
     *
     * @return The queue definition, or nullopt if not found.
     */
    std::optional<domain::queue_definition> find_by_name(context ctx,
        const std::string& name,
        const std::optional<boost::uuids::uuid>& tenant_id,
        const std::optional<boost::uuids::uuid>& party_id);

    /**
     * @brief Returns all active queue definitions visible to the current context.
     */
    std::vector<domain::queue_definition> list_active(context ctx);

    /**
     * @brief Marks a queue as inactive (soft delete).
     */
    void deactivate(context ctx, const boost::uuids::uuid& id);
};

}

#endif
