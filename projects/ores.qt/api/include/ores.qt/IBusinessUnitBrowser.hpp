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
#ifndef ORES_QT_I_BUSINESS_UNIT_BROWSER_HPP
#define ORES_QT_I_BUSINESS_UNIT_BROWSER_HPP

#include "ores.refdata.api/domain/business_unit.hpp"

namespace ores::qt {

/**
 * @brief Abstract interface for opening business unit edit/history windows.
 *
 * Decouples OrgExplorerMdiWindow (ores.qt.trading) from BusinessUnitController
 * (ores.qt.refdata), breaking the cross-plugin compile-time dependency. Named
 * on BusinessUnitController's codegen model via qt.explorer_interface.
 */
class IBusinessUnitBrowser {
public:
    virtual ~IBusinessUnitBrowser() = default;
    virtual void openEdit(const refdata::domain::business_unit& unit) = 0;
    virtual void openHistory(const refdata::domain::business_unit& unit) = 0;
};

}

#endif
