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
#include "ores.qt/BadgeMappingsTab.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include <QAbstractItemView>
#include <QHeaderView>
#include <QTabWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <algorithm>

namespace ores::qt {

BadgeMappingsTab::BadgeMappingsTab(QWidget* dialogParent)
    : QObject(dialogParent)
    , table_(new QTableWidget(dialogParent)) {
    table_->setColumnCount(2);
    table_->setHorizontalHeaderLabels({"Entity Code", "Badge"});
    table_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
    table_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    table_->verticalHeader()->setVisible(false);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->setSelectionMode(QAbstractItemView::NoSelection);

    badgeDelegate_ =
        new EntityItemDelegate({column_style::text_left, column_style::badge_centered}, table_);
    table_->setItemDelegate(badgeDelegate_);
}

void BadgeMappingsTab::attachTo(QTabWidget* tabWidget) {
    // Insert before the last (static, .ui-defined) tab -- Provenance --
    // so dynamically-attached tabs never push it out of the last slot.
    tabWidget->insertTab(std::max(0, tabWidget->count() - 1), table_, "Badge Mappings");
}

void BadgeMappingsTab::reload(const std::string& codeDomainCode, BadgeCache* badgeCache) {
    table_->setRowCount(0);

    badgeDelegate_->set_badge_color_resolver(
        1, [cache = badgeCache, codeDomainCode](const QString& value) -> badge_color_pair {
            static const badge_color_pair fallback{color_constants::badge_fallback,
                                                   color_constants::badge_fallback_text};
            if (!cache)
                return fallback;
            const auto* definition = cache->resolve(codeDomainCode, value.toStdString());
            if (!definition)
                return fallback;
            return {QColor(QString::fromStdString(definition->background_colour)),
                    QColor(QString::fromStdString(definition->text_colour))};
        });

    if (!badgeCache || codeDomainCode.empty())
        return;

    const auto mappings = badgeCache->list_by_domain(codeDomainCode);
    table_->setRowCount(static_cast<int>(mappings.size()));

    for (std::size_t row = 0; row < mappings.size(); ++row) {
        const auto& [entityCode, definition] = mappings[row];

        auto* codeItem = new QTableWidgetItem(QString::fromStdString(entityCode));
        table_->setItem(static_cast<int>(row), 0, codeItem);

        // Text is the raw entity_code, matching every other badge column
        // in the app -- the delegate's resolver keys off this same
        // display text. Friendly name/description go in the tooltip.
        auto* badgeItem = new QTableWidgetItem(QString::fromStdString(entityCode));
        if (definition) {
            badgeItem->setToolTip(
                QString::fromStdString(definition->name + " — " + definition->description));
        }
        table_->setItem(static_cast<int>(row), 1, badgeItem);
    }
}

}
