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
#ifndef ORES_QT_SCOPE_LABEL_HPP
#define ORES_QT_SCOPE_LABEL_HPP

#include "ores.synthetic.api/domain/scope.hpp"
#include <QCoreApplication>
#include <QString>

namespace ores::qt {

/**
 * @brief Human-readable label for a market_data_generation_config's scope.
 */
inline QString scopeLabel(const ores::synthetic::domain::scope& scope) {
    using ores::synthetic::domain::scope;
    switch (scope) {
        case scope::system:
            return QCoreApplication::translate("ScopeLabel", "System");
        case scope::tenant:
            return QCoreApplication::translate("ScopeLabel", "Tenant");
        case scope::party:
            return QCoreApplication::translate("ScopeLabel", "Party");
    }
    return QCoreApplication::translate("ScopeLabel", "Unknown");
}

}

#endif
