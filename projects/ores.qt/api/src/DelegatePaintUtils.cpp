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
#include "ores.qt/DelegatePaintUtils.hpp"
#include <QFontMetrics>
#include <QIcon>
#include <QPalette>
#include <QPixmap>

namespace ores::qt {

void DelegatePaintUtils::paint_centered_icon(QPainter* painter,
                                             const QStyleOptionViewItem& option,
                                             const QModelIndex& index,
                                             int padding) {
    QVariant data = index.data(Qt::DecorationRole);
    if (!data.isValid())
        return;

    auto icon = qvariant_cast<QIcon>(data);
    if (icon.isNull())
        return;

    QRect cellRect = option.rect.adjusted(padding, padding, -padding, -padding);

    // Target the view's actual decoration size (populated from
    // QAbstractItemView::iconSize(), e.g. single_flag_icon_size()/
    // currency_pair_icon_size() — shared constants every flag-bearing grid
    // sets) rather than blindly filling the cell: a column's width is
    // driven by its header/content, not by the intended icon size, so
    // filling the cell made the same flag render at a different size in
    // every entity's grid depending on how wide its icon column happened
    // to be.
    QSize targetSize = option.decorationSize.isValid() && !option.decorationSize.isEmpty()
        ? option.decorationSize
        : cellRect.size();
    targetSize = targetSize.boundedTo(cellRect.size());

    QPixmap pixmap = icon.pixmap(targetSize);
    if (pixmap.width() > targetSize.width() || pixmap.height() > targetSize.height()) {
        pixmap = pixmap.scaled(targetSize, Qt::KeepAspectRatio, Qt::SmoothTransformation);
    }

    QPoint center = option.rect.center() - pixmap.rect().center();
    if (center.x() < option.rect.left())
        center.setX(option.rect.left());
    if (center.y() < option.rect.top())
        center.setY(option.rect.top());

    painter->drawPixmap(center, pixmap);
}

void DelegatePaintUtils::apply_foreground_role(QStyleOptionViewItem& opt,
                                               const QModelIndex& index) {
    QVariant fgData = index.data(Qt::ForegroundRole);
    if (fgData.isValid()) {
        opt.palette.setColor(QPalette::Text, fgData.value<QColor>());
        opt.palette.setColor(QPalette::HighlightedText, fgData.value<QColor>());
    }
}

void DelegatePaintUtils::draw_centered_badge(QPainter* painter,
                                             const QRect& rect,
                                             const QString& text,
                                             const QColor& bg,
                                             const QColor& fg,
                                             const QFont& font) {
    painter->save();
    painter->setRenderHint(QPainter::Antialiasing, true);

    QFontMetrics fm(font);
    int textWidth = fm.horizontalAdvance(text);
    int padding = 5;
    int badgeWidth = textWidth + padding * 2;
    int badgeHeight = fm.height() + 2;

    int x = rect.center().x() - badgeWidth / 2;
    int y = rect.center().y() - badgeHeight / 2;
    QRect badgeRect(x, y, badgeWidth, badgeHeight);

    int radius = badgeHeight / 2;
    painter->setBrush(bg);
    painter->setPen(Qt::NoPen);
    painter->drawRoundedRect(badgeRect, radius, radius);

    painter->setFont(font);
    painter->setPen(fg);
    painter->drawText(badgeRect, Qt::AlignCenter, text);

    painter->restore();
}

void DelegatePaintUtils::draw_inline_badge(QPainter* painter,
                                           QRect& rect,
                                           const QString& text,
                                           const QColor& bg,
                                           const QColor& fg,
                                           const QFont& font,
                                           int padding,
                                           int spacing) {
    painter->save();
    painter->setRenderHint(QPainter::Antialiasing, true);

    QFontMetrics fm(font);
    int textWidth = fm.horizontalAdvance(text);
    int badgeWidth = textWidth + padding * 2;
    int badgeHeight = fm.height() + 2;

    int y = rect.center().y() - badgeHeight / 2;
    QRect badgeRect(rect.left(), y, badgeWidth, badgeHeight);

    int radius = badgeHeight / 2;
    painter->setBrush(bg);
    painter->setPen(Qt::NoPen);
    painter->drawRoundedRect(badgeRect, radius, radius);

    painter->setFont(font);
    painter->setPen(fg);
    painter->drawText(badgeRect, Qt::AlignCenter, text);

    painter->restore();

    // Advance rect for next badge
    rect.setLeft(rect.left() + badgeWidth + spacing);
}

}
