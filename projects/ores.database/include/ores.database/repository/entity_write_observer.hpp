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
#ifndef ORES_DATABASE_REPOSITORY_ENTITY_WRITE_OBSERVER_HPP
#define ORES_DATABASE_REPOSITORY_ENTITY_WRITE_OBSERVER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <vector>

namespace ores::database::repository {

/**
 * @brief A hook an entity's owning component may specialise to observe the
 * entity's own writes.
 *
 * The write helpers know nothing about what an entity means, so a rule that
 * belongs to one component cannot live in them. The template is the seam: a
 * component specialises it for an entity type, and the write helper asks the
 * trait with @c if constexpr. The primary template observes nothing, so an
 * entity whose component does not specialise the trait writes exactly as it
 * did before.
 *
 * @c observe runs in the same transaction as the write. It must use the
 * @p ctx it is given, and not the context the caller held, or it would leave
 * the write's transaction.
 *
 * A specialisation must be declared before the first write of its entity type
 * is instantiated. The generated repository of a trading component is the
 * translation unit that instantiates it, and it includes the component's
 * @c export.hpp header, so a component that specialises this trait includes
 * the specialisation there.
 *
 * @tparam EntityType The database entity type a write names.
 */
template <typename EntityType>
struct entity_write_observer {
    static constexpr bool observes = false;
    static void observe(context, const EntityType&, logging::logger_t&) {}
};

/**
 * @brief A batch write observes once per element.
 */
template <typename T>
struct entity_write_observer<std::vector<T>> {
    static constexpr bool observes = entity_write_observer<T>::observes;
    static void observe(context ctx, const std::vector<T>& v, logging::logger_t& lg) {
        for (const auto& entity : v)
            entity_write_observer<T>::observe(ctx, entity, lg);
    }
};

}

#endif
