/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/DatasetItemDelegate.hpp"
#include "ores.qt/BadgeCache.hpp"
#include "ores.qt/ClientDatasetModel.hpp"
#include "ores.qt/DelegatePaintUtils.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

DatasetItemDelegate::DatasetItemDelegate(BadgeCache* badgeCache, QObject* parent)
    : QStyledItemDelegate(parent),
      badgeCache_(badgeCache) {
}

void DatasetItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Check if this is the Tags column
    if (index.column() == ClientDatasetModel::Tags) {
        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        // Create badge font derived from view's font for proper scaling
        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        static const QColor default_bg(107, 114, 128);
        static const QColor white(255, 255, 255);

        auto resolve = [this](
            const QString& domain, const QString& value) -> std::pair<QColor, QColor> {
            if (badgeCache_ && !value.isEmpty()) {
                auto* def = badgeCache_->resolve(domain.toStdString(),
                                                 value.toStdString());
                if (def)
                    return {QColor(QString::fromStdString(def->background_colour)),
                            QColor(QString::fromStdString(def->text_colour))};
            }
            return {default_bg, white};
        };

        // Get the tag values
        QString origin = index.data(ClientDatasetModel::OriginRole).toString();
        QString nature = index.data(ClientDatasetModel::NatureRole).toString();
        QString treatment = index.data(ClientDatasetModel::TreatmentRole).toString();

        // Calculate starting position (left-aligned with spacing)
        QRect badgeRect = opt.rect;
        badgeRect.adjust(4, 0, 0, 0);

        if (!origin.isEmpty()) {
            auto [bg, fg] = resolve("dq_origin", origin);
            DelegatePaintUtils::draw_inline_badge(painter, badgeRect,
                origin, bg, fg, badgeFont);
        }

        if (!nature.isEmpty()) {
            auto [bg, fg] = resolve("dq_nature", nature);
            DelegatePaintUtils::draw_inline_badge(painter, badgeRect,
                nature, bg, fg, badgeFont);
        }

        if (!treatment.isEmpty()) {
            auto [bg, fg] = resolve("dq_treatment", treatment);
            DelegatePaintUtils::draw_inline_badge(painter, badgeRect,
                treatment, bg, fg, badgeFont);
        }

        return;
    }

    // Default rendering for other columns
    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

QSize DatasetItemDelegate::sizeHint(const QStyleOptionViewItem& option,
                                    const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    if (index.column() == ClientDatasetModel::Tags) {
        size.setHeight(qMax(size.height(), 24));
        size.setWidth(qMax(size.width(), 200));
    }

    return size;
}

}
