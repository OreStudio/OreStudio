/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.qt/IconUtils.hpp"

#include <QImage>
#include <QPixmap>
#include <QPainter>
#include <QSvgRenderer>

namespace ores::qt {

using namespace ores::logging;

struct IconDef {
    const char* fluent;
    const char* solar;
};

static IconDef getIconDef(Icon icon) {
    switch (icon) {
        case Icon::Add: return {"ic_fluent_add_20", "add-circle.svg"};
        case Icon::ArrowClockwise: return {"ic_fluent_arrow_clockwise_16", "restart-circle.svg"};
        case Icon::ArrowDownload: return {"ic_fluent_arrow_download_20", "download-square.svg"};
        case Icon::ArrowLeft: return {"ic_fluent_arrow_left_20", "arrow-left.svg"};
        case Icon::ArrowNext: return {"ic_fluent_arrow_next_20", "double-alt-arrow-right.svg"};
        case Icon::ArrowPrevious: return {"ic_fluent_arrow_previous_20", "double-alt-arrow-left.svg"};
        case Icon::ArrowRight: return {"ic_fluent_arrow_right_20", "arrow-right.svg"};
        case Icon::ArrowRotateCounterclockwise: return {"ic_fluent_arrow_rotate_counterclockwise_20", "undo-left-round.svg"};
        case Icon::ArrowSync: return {"ic_fluent_arrow_sync_20", "refresh-circle.svg"};
        case Icon::Book: return {"ic_fluent_book_20", "notes-minimalistic.svg"};
        case Icon::Checkmark: return {"ic_fluent_checkmark_20", "check-circle.svg"};
        case Icon::Clock: return {"ic_fluent_clock_16", "clock-circle.svg"};
        case Icon::Code: return {"ic_fluent_code_20", "code-file.svg"};
        case Icon::Currency: return {"ic_fluent_currency_dollar_euro_20", "dollar-minimalistic.svg"};
        case Icon::Database: return {"ic_fluent_database_20", "server-square.svg"};
        case Icon::Delete: return {"ic_fluent_delete_20", "trash-bin-trash.svg"};
        case Icon::DeleteDismiss: return {"ic_fluent_delete_dismiss_20", "trash-bin-trash.svg"};
        case Icon::Dismiss: return {"ic_fluent_dismiss_20", "close-circle.svg"};
        case Icon::DocumentCode: return {"ic_fluent_document_code_16", "code-file.svg"};
        case Icon::DocumentTable: return {"ic_fluent_document_table_20", "document-add.svg"};
        case Icon::Edit: return {"ic_fluent_edit_20", "pen-new-square.svg"};
        case Icon::Error: return {"ic_fluent_error_circle_20", "forbidden-circle.svg"};
        case Icon::Flag: return {"ic_fluent_flag_20", "flag.svg"};
        case Icon::Folder: return {"ic_fluent_folder_20", "folder.svg"};
        case Icon::FolderOpen: return {"ic_fluent_folder_open_20", "folder-open.svg"};
        case Icon::Globe: return {"ic_fluent_globe_20", "earth.svg"};
        case Icon::History: return {"ic_fluent_history_20", "history.svg"};
        case Icon::Info: return {"ic_fluent_info_20", "info-circle.svg"};
        case Icon::Key: return {"ic_fluent_key_20", "key-minimalistic-square.svg"};
        case Icon::KeyMultiple: return {"ic_fluent_key_multiple_20", "key-minimalistic-square.svg"};
        case Icon::Library: return {"ic_fluent_library_20", "folder.svg"};
        case Icon::LockClosed: return {"ic_fluent_lock_closed_20", "lock-password.svg"};
        case Icon::LockOpen: return {"ic_fluent_lock_open_20", "lock-unlocked.svg"};
        case Icon::NoteEdit: return {"ic_fluent_note_edit_20", "notes-minimalistic.svg"};
        case Icon::Open: return {"ic_fluent_open_20", "folder-open.svg"};
        case Icon::PasswordReset: return {"ic_fluent_password_reset_48", "lock-password-unlocked.svg"};
        case Icon::Person: return {"ic_fluent_person_20", "user-circle.svg"};
        case Icon::PersonAccounts: return {"ic_fluent_person_accounts_20", "users-group-rounded.svg"};
        case Icon::PersonAdd: return {"ic_fluent_person_add_20", "user-plus-rounded.svg"};
        case Icon::PlugConnected: return {"ic_fluent_plug_connected_20", "plug-circle.svg"};
        case Icon::PlugConnectedFilled: return {"ic_fluent_plug_connected_20", "plug-circle.svg"};
        case Icon::PlugDisconnected: return {"ic_fluent_plug_disconnected_20", "link-broken.svg"};
        case Icon::Question: return {"ic_fluent_question_20", "question-circle.svg"};
        case Icon::Record: return {"ic_fluent_record_20", "record-circle.svg"};
        case Icon::RecordFilled: return {"ic_fluent_record_20", "record-circle.svg"};
        case Icon::Save: return {"ic_fluent_save_20", "diskette.svg"};
        case Icon::Server: return {"ic_fluent_server_20", "server-square.svg"};
        case Icon::ServerLink: return {"ic_fluent_server_link_20", "server-square.svg"};
        case Icon::ServerLinkFilled: return {"ic_fluent_server_link_20", "server-square.svg"};
        case Icon::Settings: return {"ic_fluent_settings_20", "settings.svg"};
        case Icon::Star: return {"ic_fluent_star_20", "star-circle.svg"};
        case Icon::Table: return {"ic_fluent_table_20", "document-add.svg"};
        case Icon::Tag: return {"ic_fluent_tag_20", "tag.svg"};
        case Icon::Wand: return {"ic_fluent_wand_20", "magic-stick-3.svg"};
        case Icon::Warning: return {"ic_fluent_warning_20", "danger-circle.svg"};
    }
    return {};
}

QString IconUtils::iconPath(Icon icon, IconTheme theme) {
    auto def = getIconDef(icon);
    if (!def.fluent) return {};

    switch (theme) {
        case IconTheme::FluentUIRegular:
            return QString(":/icons/%1_regular.svg").arg(def.fluent);
        case IconTheme::FluentUIFilled:
            return QString(":/icons/%1_filled.svg").arg(def.fluent);
        case IconTheme::SolarizedLinear:
            return QString(":/icons/solarized/Linear/%1").arg(def.solar);
        case IconTheme::SolarizedBold:
            return QString(":/icons/solarized/Bold/%1").arg(def.solar);
    }
    return {};
}

QIcon IconUtils::createRecoloredIcon(Icon icon, const QColor& color, IconTheme theme) {
    return createRecoloredIcon(iconPath(icon, theme), color);
}

QIcon IconUtils::createRecoloredIcon(const QString& svgPath, const QColor& color) {
    QIcon originalIcon(svgPath);
    if (originalIcon.isNull()) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to load icon: "
                                  << svgPath.toStdString();
        return {};
    }

    QIcon recoloredIcon;
    // Disabled color should be visible but clearly dimmed against #1A1A1A background
    const QColor disabledColor = DisabledIconColor;

    for (int size : {16, 20, 24, 32, 48, 64}) {
        QPixmap pixmap = originalIcon.pixmap(size, size);

        // Create normal state image
        QImage normalImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < normalImage.height(); ++y) {
            for (int x = 0; x < normalImage.width(); ++x) {
                QColor pixelColor = normalImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(color.red());
                    pixelColor.setGreen(color.green());
                    pixelColor.setBlue(color.blue());
                    normalImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        recoloredIcon.addPixmap(QPixmap::fromImage(normalImage), QIcon::Normal);

        // Create disabled state image
        QImage disabledImage = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
        for (int y = 0; y < disabledImage.height(); ++y) {
            for (int x = 0; x < disabledImage.width(); ++x) {
                QColor pixelColor = disabledImage.pixelColor(x, y);
                if (pixelColor.alpha() > 0) {
                    pixelColor.setRed(disabledColor.red());
                    pixelColor.setGreen(disabledColor.green());
                    pixelColor.setBlue(disabledColor.blue());
                    disabledImage.setPixelColor(x, y, pixelColor);
                }
            }
        }
        recoloredIcon.addPixmap(QPixmap::fromImage(disabledImage), QIcon::Disabled);
    }

    return recoloredIcon;
}

QPixmap IconUtils::svgDataToPixmap(const std::string& svg_data, int height) {
    if (svg_data.empty() || height <= 0) {
        return {};
    }

    QByteArray svgBytes(svg_data.data(), static_cast<qsizetype>(svg_data.size()));
    QSvgRenderer renderer(svgBytes);

    if (!renderer.isValid()) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid SVG data, cannot render pixmap.";
        return {};
    }

    // Get SVG's default (viewBox) size to preserve aspect ratio
    QSizeF svgSize = renderer.defaultSize();
    if (svgSize.isEmpty()) {
        svgSize = QSizeF(4, 3);  // Default to 4:3 if no viewBox
    }
    qreal aspectRatio = svgSize.width() / svgSize.height();
    int width = static_cast<int>(height * aspectRatio);

    QPixmap pixmap(width, height);
    pixmap.fill(Qt::transparent);

    QPainter painter(&pixmap);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setRenderHint(QPainter::SmoothPixmapTransform);
    renderer.render(&painter);
    painter.end();

    return pixmap;
}

QIcon IconUtils::svgDataToIcon(const std::string& svg_data) {
    if (svg_data.empty()) {
        return {};
    }

    QByteArray svgBytes(svg_data.data(), static_cast<qsizetype>(svg_data.size()));
    QSvgRenderer renderer(svgBytes);

    if (!renderer.isValid()) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid SVG data, cannot render icon.";
        return {};
    }

    QIcon icon;

    // Get SVG's default (viewBox) size to preserve aspect ratio
    QSizeF svgSize = renderer.defaultSize();
    if (svgSize.isEmpty()) {
        svgSize = QSizeF(4, 3);  // Default to 4:3 if no viewBox
    }
    qreal aspectRatio = svgSize.width() / svgSize.height();

    // Render at multiple sizes for crisp display
    for (int height : {16, 20, 24, 32, 48}) {
        int width = static_cast<int>(height * aspectRatio);
        QPixmap pixmap(width, height);
        pixmap.fill(Qt::transparent);

        QPainter painter(&pixmap);
        painter.setRenderHint(QPainter::Antialiasing);
        painter.setRenderHint(QPainter::SmoothPixmapTransform);
        renderer.render(&painter);
        painter.end();

        icon.addPixmap(pixmap);
    }

    return icon;
}

}
