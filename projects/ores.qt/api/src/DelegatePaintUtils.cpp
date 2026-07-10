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
#include <QStyle>

namespace ores::qt {

namespace {

// QAbstractItemView::iconSize() is one value for the whole view, but a
// single grid can mix square single-flag columns with wide composited
// pair-flag columns (e.g. currency_pair's BaseCurrency/QuoteCurrency vs its
// PairCode column) — sized for the widest case. Trusting decorationSize's
// width directly would stretch every column's icon to that width. Instead
// take only its height as the target (the dimension every flag-bearing grid
// actually standardizes on via single_flag_icon_size()/
// currency_pair_icon_size()) and ask the icon for its own natural width at
// that height via actualSize(), so a square icon stays square and a
// composited pair icon keeps its own wider aspect ratio.
QPixmap fit_icon_pixmap(const QIcon& icon, const QStyleOptionViewItem& option, const QSize& bound) {
    int targetHeight = option.decorationSize.isValid() && option.decorationSize.height() > 0 ?
                           option.decorationSize.height() :
                           bound.height();
    QSize targetSize = icon.actualSize(QSize(bound.width(), targetHeight));
    targetSize = targetSize.boundedTo(bound);

    QPixmap pixmap = icon.pixmap(targetSize);
    if (pixmap.width() > targetSize.width() || pixmap.height() > targetSize.height()) {
        pixmap = pixmap.scaled(targetSize, Qt::KeepAspectRatio, Qt::SmoothTransformation);
    }
    return pixmap;
}

}

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
    QPixmap pixmap = fit_icon_pixmap(icon, option, cellRect.size());

    QPoint center = option.rect.center() - pixmap.rect().center();
    if (center.x() < option.rect.left())
        center.setX(option.rect.left());
    if (center.y() < option.rect.top())
        center.setY(option.rect.top());

    painter->drawPixmap(center, pixmap);
}

void DelegatePaintUtils::paint_icon_and_text(QPainter* painter,
                                             const QStyleOptionViewItem& option,
                                             const QModelIndex& index,
                                             int padding,
                                             int spacing) {
    QRect rect = option.rect.adjusted(padding, padding, -padding, -padding);

    QVariant data = index.data(Qt::DecorationRole);
    QIcon icon = data.isValid() ? qvariant_cast<QIcon>(data) : QIcon();
    if (!icon.isNull()) {
        QPixmap pixmap = fit_icon_pixmap(icon, option, rect.size());
        QPoint pos(rect.left(), rect.center().y() - pixmap.height() / 2);
        painter->drawPixmap(pos, pixmap);
        rect.setLeft(rect.left() + pixmap.width() + spacing);
    }

    const QString text = index.data(Qt::DisplayRole).toString();
    if (!text.isEmpty()) {
        painter->save();
        if (option.state & QStyle::State_Selected)
            painter->setPen(option.palette.color(QPalette::HighlightedText));
        else
            painter->setPen(option.palette.color(QPalette::Text));
        painter->setFont(option.font);
        painter->drawText(rect, Qt::AlignLeft | Qt::AlignVCenter, text);
        painter->restore();
    }
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
