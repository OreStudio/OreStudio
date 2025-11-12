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
#ifndef ORES_QT_ABOUT_DIALOG_HPP
#define ORES_QT_ABOUT_DIALOG_HPP

#include <QDialog>
#include <memory>
#include "ui_AboutDialog.h"
#include "ores.utility/log/make_logger.hpp"

namespace Ui {

class AboutDialog;

}

namespace ores::qt {

/**
 * @brief About dialog showing application version and build information.
 */
class AboutDialog : public QDialog {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.about_dialog");
        return instance;
    }

public:
    explicit AboutDialog(QWidget* parent = nullptr);
    ~AboutDialog();

private:
    void setupVersionInfo();

private:
    std::unique_ptr<Ui::AboutDialog> ui_;
};

}

#endif
