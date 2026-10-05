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
#ifndef ORES_REFDATA_CORE_SERVICE_CURVE_CONFIGURATION_DOCUMENT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURVE_CONFIGURATION_DOCUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>

namespace ores::refdata::service {

/**
 * @brief Stores and reads ORE curve configuration documents.
 *
 * A save stamps the session's party on every row when the session has one. A
 * read selects one document's rows under the session, so row level security
 * confines it to what the session's party may see.
 */
class ORES_REFDATA_CORE_EXPORT curve_configuration_document_service {
public:
    using context = ores::database::context;

    explicit curve_configuration_document_service(context ctx);

    void save(messaging::curve_configuration_document v);

    /**
     * @brief The document whose header has @p id.
     */
    messaging::curve_configuration_document get(const boost::uuids::uuid& id);

    /**
     * @brief Deletes the document whose header has @p id, its children first.
     *
     * Each table is its own write, not one transaction, so a failure part way
     * leaves the rows not yet deleted.
     */
    void remove(const boost::uuids::uuid& id);

    /**
     * @brief The header id of the document a reporting configuration names.
     */
    std::optional<boost::uuids::uuid>
    find_by_configuration(const boost::uuids::uuid& configuration_id);

private:
    context ctx_;
};

}

#endif
