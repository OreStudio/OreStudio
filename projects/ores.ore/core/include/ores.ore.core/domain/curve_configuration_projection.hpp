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
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_DOMAIN_CURVE_CONFIGURATION_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_CURVE_CONFIGURATION_PROJECTION_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/generated/curve_configuration.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include <string>
#include <string_view>

namespace ores::ore::domain {

/**
 * @brief Projects an ORE CurveConfiguration record into the refdata curve
 * entities, and back.
 *
 * Every curve section and the document's report configuration are projected.
 * Elements of an entry the projection cannot hold yet are refused rather than
 * dropped, because a dropped element would pass as a round trip while losing
 * data.
 *
 * Segments are restored in the order the document wrote them within each
 * segment element. The record keeps one list per element, so the order across
 * elements is not part of the parsed document.
 */
class ORES_ORE_CORE_EXPORT curve_configuration_projection {
public:
    using document = xml::generated::curve_configuration::curveconfiguration;

    /**
     * @brief Projects an ORE CurveConfiguration document into the refdata
     * entities.
     *
     * @throws std::runtime_error for a report configuration or a section with
     * entries the projection does not model yet.
     */
    static ores::refdata::messaging::curve_configuration_document map(const document& v);

    /**
     * @brief Reconstructs an ORE CurveConfiguration document from projected
     * entities.
     *
     * @throws std::runtime_error for a row the document has no place for: an
     * unknown section or segment type, a detail or child row whose parent is
     * absent, or a quote directly on an entry whose section holds quotes only
     * on its segments.
     */
    static document reverse(const ores::refdata::messaging::curve_configuration_document& v);

    /**
     * @brief Reads a CurveConfiguration document into the record the table
     * describes.
     *
     * @param text The document.
     * @param origin The file the text came from, named in the failure.
     * @throws std::invalid_argument naming the origin and the line where the
     * document does not bind.
     */
    static document read(std::string_view text, std::string_view origin = {});

    /**
     * @brief Writes a CurveConfiguration record as a document.
     *
     * @throws std::runtime_error when the record cannot produce one.
     */
    static std::string write(const document& v);
};

}

#endif
