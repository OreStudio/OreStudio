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
#include "ores.qt/ClientDatasetModel.hpp"
#include "ores.qt/ColorConstants.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

using dbc = dimension_badge_colors;

QColor getOriginColor(const QString& origin) {
    if (origin == "Primary") return dbc::origin_primary;
    if (origin == "Derived") return dbc::origin_derived;
    return dbc::default_bg;
}

QColor getNatureColor(const QString& nature) {
    if (nature == "Actual") return dbc::nature_actual;
    if (nature == "Estimated") return dbc::nature_estimated;
    if (nature == "Simulated") return dbc::nature_simulated;
    return dbc::default_bg;
}

QColor getTreatmentColor(const QString& treatment) {
    if (treatment == "Raw") return dbc::treatment_raw;
    if (treatment == "Cleaned") return dbc::treatment_cleaned;
    if (treatment == "Enriched") return dbc::treatment_enriched;
    return dbc::default_bg;
}

}

DatasetItemDelegate::DatasetItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
}

void DatasetItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Check if this is the Tags column
    if (index.column() == ClientDatasetModel::Tags) {
        // Draw the background (for selection highlighting)
        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        // Create badge font derived from view's font for proper scaling
        QFont badgeFont = opt.font;
        badgeFont.setPointSize(qRound(badgeFont.pointSize() * 0.8));
        badgeFont.setBold(true);

        // Get the tag values
        QString origin = index.data(ClientDatasetModel::OriginRole).toString();
        QString nature = index.data(ClientDatasetModel::NatureRole).toString();
        QString treatment = index.data(ClientDatasetModel::TreatmentRole).toString();

        // Calculate starting position (left-aligned with spacing)
        QRect badgeRect = opt.rect;
        badgeRect.adjust(4, 0, 0, 0);

        // Draw Origin badge
        if (!origin.isEmpty()) {
            drawBadge(painter, badgeRect, origin, getOriginColor(origin), dbc::text, badgeFont);
        }

        // Draw Nature badge
        if (!nature.isEmpty()) {
            drawBadge(painter, badgeRect, nature, getNatureColor(nature), dbc::text, badgeFont);
        }

        // Draw Treatment badge
        if (!treatment.isEmpty()) {
            drawBadge(painter, badgeRect, treatment, getTreatmentColor(treatment), dbc::text, badgeFont);
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

void DatasetItemDelegate::drawBadge(QPainter* painter, QRect& rect,
                                    const QString& text, const QColor& backgroundColor,
                                    const QColor& textColor, const QFont& font) const {
    painter->save();
    painter->setRenderHint(QPainter::Antialiasing, true);

    // Calculate badge dimensions
    QFontMetrics fm(font);
    int textWidth = fm.horizontalAdvance(text);
    int padding = 4;
    int badgeWidth = textWidth + padding * 2;
    int badgeHeight = fm.height() + 2;
    int spacing = 3;

    // Position badge vertically centered
    int y = rect.center().y() - badgeHeight / 2;
    QRect badgeRect(rect.left(), y, badgeWidth, badgeHeight);

    // Draw pill-shaped background
    int radius = badgeHeight / 2;
    painter->setBrush(backgroundColor);
    painter->setPen(Qt::NoPen);
    painter->drawRoundedRect(badgeRect, radius, radius);

    // Draw text
    painter->setFont(font);
    painter->setPen(textColor);
    painter->drawText(badgeRect, Qt::AlignCenter, text);

    painter->restore();

    // Move rect for next badge
    rect.setLeft(rect.left() + badgeWidth + spacing);
}

}
