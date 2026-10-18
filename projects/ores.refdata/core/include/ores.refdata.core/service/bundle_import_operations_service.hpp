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
#ifndef ORES_REFDATA_CORE_SERVICE_BUNDLE_IMPORT_OPERATIONS_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BUNDLE_IMPORT_OPERATIONS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/messaging/bundle_import_operations_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/bundle_import_item_repository.hpp"
#include "ores.refdata.core/repository/bundle_import_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Begins, stages and commits a bundle import.
 *
 * The bundle is too large for one message, so the operations carry one family
 * at a time. A begin opens a durable session, a stage writes one family payload
 * against it, and a commit applies every staged payload in one transaction.
 *
 * The staging service is hand-written beside the generated per-entity service,
 * because these three operations are not the entity's common verbs and each one
 * decides something the generated service cannot: the state a begin stamps, the
 * family the registry knows, and the transaction a commit holds.
 */
class ORES_REFDATA_CORE_EXPORT bundle_import_operations_service {
public:
    using context = ores::database::context;

    explicit bundle_import_operations_service(context ctx);

    /**
     * @brief Opens a staging session, or answers the one the id already names.
     *
     * Refuses a caller that states the state, a nil id, and a session with no
     * party. A repeated begin with a known id answers the existing import and
     * writes nothing.
     */
    messaging::begin_bundle_import_response
    begin(const messaging::begin_bundle_import_request& request);

    /**
     * @brief Stages one family payload against an open import.
     *
     * Refuses a family the registry does not know, an import that is not
     * staging, a nil import id, a negative sequence, and a repeated
     * (import, family, sequence), which would otherwise be a second row.
     */
    messaging::stage_bundle_family_response
    stage(const messaging::stage_bundle_family_request& request);

    /**
     * @brief Applies every staged family of one import in one transaction.
     *
     * Locks the import row, refuses anything but a staging import, builds one
     * bundle from the staged rows, applies it, marks the import applied, and
     * commits. Any failure rolls the transaction back and leaves the import
     * staging.
     */
    messaging::commit_bundle_import_response
    commit(const messaging::commit_bundle_import_request& request);

private:
    context ctx_;
    repository::bundle_import_repository imports_;
    repository::bundle_import_item_repository items_;
};

}

#endif
