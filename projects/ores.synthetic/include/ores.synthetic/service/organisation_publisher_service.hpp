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
#ifndef ORES_SYNTHETIC_SERVICE_ORGANISATION_PUBLISHER_SERVICE_HPP
#define ORES_SYNTHETIC_SERVICE_ORGANISATION_PUBLISHER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic/domain/generated_organisation.hpp"
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"

namespace ores::synthetic::service {

/**
 * @brief Persists a generated organisation to the database.
 *
 * Takes a generated_organisation and writes all entities to the database
 * using the refdata repositories. Entities are written in FK order to
 * satisfy foreign key constraints.
 */
class organisation_publisher_service final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.synthetic.service.organisation_publisher_service");
        return instance;
    }

public:
    explicit organisation_publisher_service(database::context ctx);

    /**
     * @brief Publish a generated organisation to the database.
     *
     * @param org The generated organisation to persist.
     * @return Response with success/failure and entity counts.
     */
    messaging::generate_organisation_response publish(
        const domain::generated_organisation& org);

private:
    /**
     * @brief Seeds the minimum currencies required by the generated
     * organisation into the system tenant's production table.
     *
     * The portfolio and book insert triggers validate currencies against
     * ores_refdata_currencies_tbl for the system tenant. If the system
     * tenant has not had currencies published via the DQ pipeline, the
     * generated organisation cannot be inserted. This method ensures the
     * required currencies (GBP, USD, EUR, JPY) exist.
     */
    void seed_currencies();

    database::context ctx_;
};

}

#endif
