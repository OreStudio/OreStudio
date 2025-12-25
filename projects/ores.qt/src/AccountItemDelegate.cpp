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
#include "ores.qt/AccountItemDelegate.hpp"
#include "ores.qt/ClientAccountModel.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

// Column indices (matches ClientAccountModel::Column enum)
constexpr int status_column_index = 2;
constexpr int locked_column_index = 3;
constexpr int version_column_index = 4;
constexpr int recorded_by_column_index = 5;
constexpr int recorded_at_column_index = 6;

// Login status badge colors
const QColor status_never_bg(107, 114, 128);    // Gray for never logged in
const QColor status_never_text(255, 255, 255);
const QColor status_old_bg(234, 179, 8);        // Amber/Yellow for long ago
const QColor status_old_text(255, 255, 255);
const QColor status_recent_bg(59, 130, 246);    // Blue for recent
const QColor status_recent_text(255, 255, 255);
const QColor status_online_bg(34, 197, 94);     // Green for online
const QColor status_online_text(255, 255, 255);

// Locked status badge colors
const QColor locked_badge_bg(239, 68, 68);      // Red background for locked
const QColor locked_badge_text(255, 255, 255);  // White text
const QColor unlocked_badge_bg(107, 114, 128);  // Gray background for not locked
const QColor unlocked_badge_text(255, 255, 255); // White text

}

AccountItemDelegate::AccountItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    monospaceFont_ = QFont("Fira Code");
    monospaceFont_.setPointSize(10);

    badgeFont_ = QFont();
    badgeFont_.setPointSize(7);
    badgeFont_.setBold(true);
}

void AccountItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Handle badge columns
    if (index.column() == status_column_index ||
        index.column() == locked_column_index) {

        // Draw the background (for selection highlighting)
        QStyle* style = QApplication::style();
        style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

        // Get the display text to determine badge type
        QString text = index.data(Qt::DisplayRole).toString();
        QColor bgColor, textColor;
        QString badgeText;

        if (index.column() == status_column_index) {
            // Login status column - different color for each status
            if (text == "Online") {
                bgColor = status_online_bg;
                textColor = status_online_text;
                badgeText = tr("Online");
            } else if (text == "Recent") {
                bgColor = status_recent_bg;
                textColor = status_recent_text;
                badgeText = tr("Recent");
            } else if (text == "Old") {
                bgColor = status_old_bg;
                textColor = status_old_text;
                badgeText = tr("Old");
            } else {
                // Never logged in
                bgColor = status_never_bg;
                textColor = status_never_text;
                badgeText = tr("Never");
            }
        } else if (index.column() == locked_column_index) {
            // Locked column
            if (text == "Locked") {
                bgColor = locked_badge_bg;
                textColor = locked_badge_text;
                badgeText = tr("Yes");
            } else {
                bgColor = unlocked_badge_bg;
                textColor = unlocked_badge_text;
                badgeText = tr("No");
            }
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
        case 0: // Username
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case 1: // Email
            opt.displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
            break;
        case version_column_index: // Version
            opt.font = monospaceFont_;
            opt.displayAlignment = Qt::AlignCenter;
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

QSize AccountItemDelegate::sizeHint(const QStyleOptionViewItem& option,
                                    const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    // Ensure minimum height for badge columns
    if (index.column() == status_column_index ||
        index.column() == locked_column_index) {
        size.setHeight(qMax(size.height(), 24));
        // Status column needs more width for "Recent" and "Online" text
        if (index.column() == status_column_index) {
            size.setWidth(qMax(size.width(), 70));
        } else {
            size.setWidth(qMax(size.width(), 50));
        }
    }

    return size;
}

void AccountItemDelegate::drawBadge(QPainter* painter, const QRect& rect,
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
