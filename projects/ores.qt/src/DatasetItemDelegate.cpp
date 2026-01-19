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

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

// Badge colors for Origin dimension
const QColor origin_primary_bg(59, 130, 246);    // Blue for Primary
const QColor origin_derived_bg(139, 92, 246);    // Purple for Derived

// Badge colors for Nature dimension
const QColor nature_actual_bg(34, 197, 94);      // Green for Actual
const QColor nature_estimated_bg(234, 179, 8);   // Amber for Estimated
const QColor nature_simulated_bg(236, 72, 153);  // Pink for Simulated

// Badge colors for Treatment dimension
const QColor treatment_raw_bg(107, 114, 128);    // Gray for Raw
const QColor treatment_cleaned_bg(14, 165, 233); // Sky blue for Cleaned
const QColor treatment_enriched_bg(168, 85, 247);// Purple for Enriched

const QColor badge_text(255, 255, 255);

QColor getOriginColor(const QString& origin) {
    if (origin == "Primary") return origin_primary_bg;
    if (origin == "Derived") return origin_derived_bg;
    return QColor(107, 114, 128);
}

QColor getNatureColor(const QString& nature) {
    if (nature == "Actual") return nature_actual_bg;
    if (nature == "Estimated") return nature_estimated_bg;
    if (nature == "Simulated") return nature_simulated_bg;
    return QColor(107, 114, 128);
}

QColor getTreatmentColor(const QString& treatment) {
    if (treatment == "Raw") return treatment_raw_bg;
    if (treatment == "Cleaned") return treatment_cleaned_bg;
    if (treatment == "Enriched") return treatment_enriched_bg;
    return QColor(107, 114, 128);
}

}

DatasetItemDelegate::DatasetItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    badgeFont_ = QFont();
    badgeFont_.setPointSize(7);
    badgeFont_.setBold(true);
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

        // Get the tag values
        QString origin = index.data(ClientDatasetModel::OriginRole).toString();
        QString nature = index.data(ClientDatasetModel::NatureRole).toString();
        QString treatment = index.data(ClientDatasetModel::TreatmentRole).toString();

        // Calculate starting position (left-aligned with spacing)
        QRect badgeRect = opt.rect;
        badgeRect.adjust(4, 0, 0, 0);

        // Draw Origin badge
        if (!origin.isEmpty()) {
            drawBadge(painter, badgeRect, origin, getOriginColor(origin), badge_text);
        }

        // Draw Nature badge
        if (!nature.isEmpty()) {
            drawBadge(painter, badgeRect, nature, getNatureColor(nature), badge_text);
        }

        // Draw Treatment badge
        if (!treatment.isEmpty()) {
            drawBadge(painter, badgeRect, treatment, getTreatmentColor(treatment), badge_text);
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
                                    const QColor& textColor) const {
    painter->save();
    painter->setRenderHint(QPainter::Antialiasing, true);

    // Calculate badge dimensions
    QFontMetrics fm(badgeFont_);
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
    painter->setFont(badgeFont_);
    painter->setPen(textColor);
    painter->drawText(badgeRect, Qt::AlignCenter, text);

    painter->restore();

    // Move rect for next badge
    rect.setLeft(rect.left() + badgeWidth + spacing);
}

}
