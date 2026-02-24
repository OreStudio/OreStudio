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
#ifndef ORES_QT_ICON_UTILS_HPP
#define ORES_QT_ICON_UTILS_HPP

#include <QIcon>
#include <QColor>
#include <QString>
#include <optional>
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Enumeration of available application icons by semantic function.
 */
enum class Icon {
    Add,
    ArrowClockwise,
    ArrowDownload,
    ArrowLeft,
    ArrowNext,
    ArrowPrevious,
    ArrowRight,
    ArrowRotateCounterclockwise,
    ArrowSync,
    Book,
    BookOpen,
    Briefcase,
    Building,
    BuildingBank,
    BuildingSkyscraper,
    Chart,
    Checkmark,
    Classification,
    Clock,
    Code,
    ContactCard,
    Copy,
    Currency,
    Database,
    Delete,
    DeleteDismiss,
    Dismiss,
    DocumentCode,
    DocumentTable,
    Edit,
    Error,
    ExportCsv,
    ExportFpml,
    ExportOre,
    Flag,
    Folder,
    FolderOpen,
    Globe,
    Handshake,
    History,
    Histogram,
    ImportCsv,
    ImportFpml,
    ImportOre,
    Info,
    Key,
    KeyMultiple,
    Keyboard,
    Library,
    LockClosed,
    LockOpen,
    NoteEdit,
    Open,
    Organization,
    PasswordReset,
    PeopleTeam,
    Person,
    PersonAccounts,
    PersonAdd,
    PlugConnected,
    PlugConnectedFilled,
    PlugDisconnected,
    Publish,
    Question,
    Record,
    RecordFilled,
    Save,
    Server,
    ServerLink,
    ServerLinkFilled,
    Settings,
    Star,
    Table,
    Tag,
    Terminal,
    Wand,
    Warning
};

/**
 * @brief Enumeration of available icon themes/styles.
 */
enum class IconTheme {
    FluentUIRegular,
    FluentUIFilled,
    SolarizedLinear,
    SolarizedBold
};

/**
 * @brief Utility class for icon manipulation operations.
 */
class IconUtils {
public:
    // Common icon colors
    static inline const QColor DefaultIconColor{220, 220, 220};
    static inline const QColor ConnectedColor{100, 200, 100};
    static inline const QColor DisconnectedColor{200, 100, 100};
    static inline const QColor ReconnectingColor{230, 180, 80};
    static inline const QColor RecordingOnColor{220, 80, 80};
    static inline const QColor DisabledIconColor{100, 100, 100};

private:
    static inline IconTheme currentTheme_{IconTheme::FluentUIRegular};
    inline static std::string_view logger_name = "ores.qt.icon_utils";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Sets the global icon theme.
     * @param theme The theme to use globally.
     */
    static void setTheme(IconTheme theme) { currentTheme_ = theme; }

    /**
     * @brief Gets the current global icon theme.
     * @return The current global theme.
     */
    static IconTheme currentTheme() { return currentTheme_; }

    /**
     * @brief Gets the resource path for a semantic icon using the current global theme.
     * @param icon The semantic icon identifier
     * @return The resource path string (e.g. ":/icons/...")
     */
    static QString iconPath(Icon icon);

    /**
     * @brief Creates a recolored version of a semantic icon using the current global theme.
     * @param icon The semantic icon identifier
     * @param color Color to apply to the icon in normal state
     * @return QIcon with recolored normal and disabled states
     */
    static QIcon createRecoloredIcon(Icon icon, const QColor& color);

    /**
     * @brief Creates a recolored version of an SVG icon.
     *
     * Loads an SVG icon and creates a new QIcon with the specified color
     * for normal state and a dark gray color for disabled state. Uses
     * QPainter composition modes for efficient rendering.
     *
     * @param svgPath Path to the SVG icon resource (e.g., ":/icons/icon.svg")
     * @param color Color to apply to the icon in normal state
     * @return QIcon with recolored normal and disabled states, or empty icon on failure
     */
    static QIcon createRecoloredIcon(const QString& svgPath, const QColor& color);

    /**
     * @brief Renders SVG data to a QIcon preserving aspect ratio.
     *
     * Creates a QIcon from raw SVG data string, rendering at multiple sizes
     * while preserving the SVG's native aspect ratio (from viewBox).
     *
     * @param svg_data Raw SVG content as a string
     * @return QIcon rendered from the SVG, or empty icon on failure
     */
    static QIcon svgDataToIcon(const std::string& svg_data);

    /**
     * @brief Renders SVG data to a QPixmap at specified height, preserving aspect ratio.
     *
     * @param svg_data Raw SVG content as a string
     * @param height Target height in pixels (width computed from aspect ratio)
     * @return QPixmap rendered from the SVG, or null pixmap on failure
     */
    static QPixmap svgDataToPixmap(const std::string& svg_data, int height);
};

}

#endif
