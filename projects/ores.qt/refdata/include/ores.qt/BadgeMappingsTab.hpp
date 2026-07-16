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
#ifndef ORES_QT_BADGE_MAPPINGS_TAB_HPP
#define ORES_QT_BADGE_MAPPINGS_TAB_HPP

#include <QObject>
#include <string>

class QTabWidget;
class QTableWidget;
class QWidget;

namespace ores::qt {

class BadgeCache;
class EntityItemDelegate;

/**
 * @brief Owns and drives the "Badge Mappings" tab embedded in
 * CodeDomainDetailDialog, showing every entity_code -> badge_definition
 * mapping registered for the displayed code_domain. Hand-written (this
 * dialog is itself hand-maintained, not codegen'd).
 *
 * badge_mapping is a junction model with no Qt facet of its own (junction
 * codegen deliberately stops at domain/generator/repository) and its data
 * is already held client-side by BadgeCache, loaded once at startup -- this
 * class only renders it, no network fetch of its own.
 *
 * BadgeCache is passed to reload() rather than the constructor: the tab is
 * attached during the dialog's own construction, before the dialog's
 * BadgeCache pointer (set separately by the controller) is available.
 */
class BadgeMappingsTab final : public QObject {
    Q_OBJECT

public:
    explicit BadgeMappingsTab(QWidget* dialogParent);

    /** @brief Add the tab to the dialog's tab widget. Call once, from the constructor. */
    void attachTo(QTabWidget* tabWidget);

    /** @brief Repopulate the table with codeDomainCode's mappings from badgeCache. */
    void reload(const std::string& codeDomainCode, BadgeCache* badgeCache);

private:
    QTableWidget* table_{nullptr};
    EntityItemDelegate* badgeDelegate_{nullptr};
};

}

#endif
