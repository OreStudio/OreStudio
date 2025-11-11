/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_MDI_AREA_WITH_BACKGROUND_HPP
#define ORES_QT_MDI_AREA_WITH_BACKGROUND_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <QMdiArea>
#include <QPixmap>
#include "ores.utility/log/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Custom QMdiArea that displays a background logo when no windows are open.
 */
class MdiAreaWithBackground : public QMdiArea {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.mdi_area_with_background");
        return instance;
    }

public:
    explicit MdiAreaWithBackground(QWidget* parent = nullptr);
    void setBackgroundLogo(const QString& imagePath);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    QPixmap backgroundLogo_;
};

}

#endif
