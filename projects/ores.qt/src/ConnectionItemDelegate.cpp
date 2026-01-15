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
#include "ores.qt/ConnectionItemDelegate.hpp"
#include "ores.qt/ConnectionTreeModel.hpp"
#include "ores.qt/ConnectionTypes.hpp"

#include <QPainter>
#include <QApplication>
#include <QStyleOptionViewItem>

namespace ores::qt {

namespace {

constexpr int badge_spacing = 4;
constexpr int badge_padding = 4;
constexpr int text_badge_gap = 8;

}

ConnectionItemDelegate::ConnectionItemDelegate(QObject* parent)
    : QStyledItemDelegate(parent) {
    badgeFont_ = QFont();
    badgeFont_.setPointSize(7);
    badgeFont_.setBold(true);
}

void ConnectionItemDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option,
                                    const QModelIndex& index) const {
    QStyleOptionViewItem opt = option;
    initStyleOption(&opt, index);

    // Only custom render the Name column (column 0) for environments
    if (index.column() != 0) {
        QStyledItemDelegate::paint(painter, option, index);
        return;
    }

    // Check if this is an environment node with tags
    QVariant tagsData = index.data(ConnectionTreeModel::TagsRole);
    if (!tagsData.isValid() || tagsData.toStringList().isEmpty()) {
        // No tags, use default rendering
        QStyledItemDelegate::paint(painter, option, index);
        return;
    }

    QStringList tags = tagsData.toStringList();

    // Draw the background (for selection highlighting)
    QStyle* style = QApplication::style();
    style->drawPrimitive(QStyle::PE_PanelItemViewItem, &opt, painter);

    painter->save();

    // Draw the icon if present
    int iconWidth = 0;
    if (!opt.icon.isNull()) {
        QRect iconRect = opt.rect;
        iconRect.setWidth(opt.decorationSize.width());
        opt.icon.paint(painter, iconRect, Qt::AlignLeft | Qt::AlignVCenter);
        iconWidth = opt.decorationSize.width() + 4;
    }

    // Calculate text area
    QRect textRect = opt.rect;
    textRect.setLeft(textRect.left() + iconWidth);

    // Draw the text
    QString text = opt.text;
    QFontMetrics fm(opt.font);
    int textWidth = fm.horizontalAdvance(text);

    painter->setFont(opt.font);
    if (opt.state & QStyle::State_Selected) {
        painter->setPen(opt.palette.color(QPalette::HighlightedText));
    } else {
        painter->setPen(opt.palette.color(QPalette::Text));
    }

    QRect actualTextRect = textRect;
    actualTextRect.setWidth(textWidth);
    painter->drawText(actualTextRect, Qt::AlignLeft | Qt::AlignVCenter, text);

    // Draw tag badges after the text
    int badgeX = textRect.left() + textWidth + text_badge_gap;
    int badgeY = textRect.center().y();

    QFontMetrics badgeFm(badgeFont_);
    int badgeHeight = badgeFm.height() + 2;

    for (const QString& tag : tags) {
        int tagTextWidth = badgeFm.horizontalAdvance(tag);
        int badgeWidth = tagTextWidth + badge_padding * 2;

        QRect badgeRect(badgeX, badgeY - badgeHeight / 2, badgeWidth, badgeHeight);

        // Only draw if badge fits in the visible area
        if (badgeRect.right() > textRect.right()) {
            break;
        }

        drawTagBadge(painter, badgeRect, tag, colorForTag(tag));
        badgeX += badgeWidth + badge_spacing;
    }

    painter->restore();
}

QSize ConnectionItemDelegate::sizeHint(const QStyleOptionViewItem& option,
                                        const QModelIndex& index) const {
    QSize size = QStyledItemDelegate::sizeHint(option, index);

    // Ensure minimum height for rows with tags
    if (index.column() == 0) {
        QVariant tagsData = index.data(ConnectionTreeModel::TagsRole);
        if (tagsData.isValid() && !tagsData.toStringList().isEmpty()) {
            size.setHeight(qMax(size.height(), 26));
        }
    }

    return size;
}

void ConnectionItemDelegate::drawTagBadge(QPainter* painter, const QRect& rect,
                                           const QString& text, const QColor& backgroundColor) const {
    painter->save();
    painter->setRenderHint(QPainter::Antialiasing, true);

    // Draw pill-shaped background
    int radius = rect.height() / 2;
    painter->setBrush(backgroundColor);
    painter->setPen(Qt::NoPen);
    painter->drawRoundedRect(rect, radius, radius);

    // Draw text
    painter->setFont(badgeFont_);
    painter->setPen(Qt::white);
    painter->drawText(rect, Qt::AlignCenter, text);

    painter->restore();
}

}
