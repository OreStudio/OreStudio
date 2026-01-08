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
#include "ores.qt/FeatureFlagItemDelegate.hpp"
#include "ores.qt/ClientFeatureFlagModel.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

// Column indices (matches ClientFeatureFlagModel::Column enum)
constexpr int name_column_index = ClientFeatureFlagModel::Name;
constexpr int enabled_column_index = ClientFeatureFlagModel::Enabled;
constexpr int version_column_index = ClientFeatureFlagModel::Version;
constexpr int recorded_by_column_index = ClientFeatureFlagModel::RecordedBy;
constexpr int recorded_at_column_index = ClientFeatureFlagModel::RecordedAt;

// Enabled status badge colors
const QColor enabled_badge_bg(34, 197, 94);       // Green background for enabled
const QColor enabled_badge_text(255, 255, 255);   // White text
const QColor disabled_badge_bg(107, 114, 128);    // Gray background for disabled
const QColor disabled_badge_text(255, 255, 255);  // White text

}

FeatureFlagItemDelegate::FeatureFlagItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = QFont("Fira Code");
    monospaceFont_.setPointSize(10);

    badgeFont_ = QFont();
    badgeFont_.setPointSize(7);
    badgeFont_.setBold(true);
}

void FeatureFlagItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                    const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Handle badge column for Enabled
    if (index.column() == enabled_column_index) {
        // Draw the background (for selection highlighting)
        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        // Get the display text to determine badge type
        QString text = index.data(Qt::DisplayRole).toString();
        QColor bgColor, textColor;
        QString badgeText;

        if (text == "Yes") {
            bgColor = enabled_badge_bg;
            textColor = enabled_badge_text;
            badgeText = tr("Yes");
        } else {
            bgColor = disabled_badge_bg;
            textColor = disabled_badge_text;
            badgeText = tr("No");
        }

        // Draw the badge
        drawBadge(painter, opt.rect, badgeText, bgColor, textColor);
        return;
    }

    // Check if model provides a custom foreground color (e.g., recency coloring)
    QVariant fgData = index.data(Qt::ForegroundRole);
    if (fgData.isValid()) {
        opt.palette.setColor(QPalette::Text, fgData.value<QColor>());
        opt.palette.setColor(QPalette::HighlightedText, fgData.value<QColor>());
    }

    // Apply formatting based on column
    switch (index.column()) {
        case name_column_index: // Name
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case recorded_by_column_index: // RecordedBy
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case recorded_at_column_index: // RecordedAt
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        default:
            break;
    }

    // Draw the item using the modified options
    QApplication::style()->drawControl(QStyle::CE_ItemViewItem, &opt, painter);
}

QSize FeatureFlagItemDelegate::sizeHint(const QStyleOptionViewItem& option,
                                        const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    // Ensure minimum height for badge columns
    if (index.column() == enabled_column_index) {
        size.setHeight(qMax(size.height(), 24));
        size.setWidth(qMax(size.width(), 50));
    }

    return size;
}

void FeatureFlagItemDelegate::drawBadge(QPainter* painter, const QRect& rect,
                                        const QString& text, const QColor& backgroundColor,
                                        const QColor& textColor) const {
    painter->save();
    painter->setRenderHint(QPainter::Antialiasing, true);

    // Calculate badge dimensions (compact size)
    QFontMetrics fm(badgeFont_);
    int textWidth = fm.horizontalAdvance(text);
    int padding = 5;
    int badgeWidth = textWidth + padding * 2;
    int badgeHeight = fm.height() + 2;

    // Center the badge in the cell
    int x = rect.center().x() - badgeWidth / 2;
    int y = rect.center().y() - badgeHeight / 2;
    QRect badgeRect(x, y, badgeWidth, badgeHeight);

    // Draw pill-shaped background (rounded rectangle)
    int radius = badgeHeight / 2;
    painter->setBrush(backgroundColor);
    painter->setPen(Qt::NoPen);
    painter->drawRoundedRect(badgeRect, radius, radius);

    // Draw text
    painter->setFont(badgeFont_);
    painter->setPen(textColor);
    painter->drawText(badgeRect, Qt::AlignCenter, text);

    painter->restore();
}

}
