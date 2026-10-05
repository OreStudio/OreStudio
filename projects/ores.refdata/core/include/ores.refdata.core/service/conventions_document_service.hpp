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
#ifndef ORES_REFDATA_CORE_SERVICE_CONVENTIONS_DOCUMENT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CONVENTIONS_DOCUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief What a conventions save did not store, and why.
 */
struct conventions_save_result {
    /**
     * World conventions the tenant already held. A save never changes world
     * data, so the tenant's row stands.
     */
    std::vector<std::string> world_kept;
    /**
     * FX conventions, by ORE id, which are world data with no store of their
     * own yet.
     */
    std::vector<std::string> fx_skipped;
};

/**
 * @brief Stores and reads ORE conventions documents.
 *
 * The instrument conventions belong to a party and replace any the party holds
 * under the same id. The index conventions are world data: one the tenant
 * lacks is added, and one it holds is left as it is.
 */
class ORES_REFDATA_CORE_EXPORT conventions_document_service {
public:
    using context = ores::database::context;

    explicit conventions_document_service(context ctx);

    conventions_save_result save(messaging::conventions_document v);

    /**
     * @brief Every convention the session sees: the party's instrument
     * conventions and the tenant's index conventions.
     */
    messaging::conventions_document get();

private:
    context ctx_;
};

}

#endif
