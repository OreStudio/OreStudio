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
#ifndef ORES_QT_DETACHABLE_MDI_SUB_WINDOW_HPP
#define ORES_QT_DETACHABLE_MDI_SUB_WINDOW_HPP

#include <QMdiSubWindow>
#include <QMdiArea>
#include <QPointer>
#include <QPoint>
#include <QSize>
#include "ores.utility/log/make_logger.hpp"

namespace ores::qt {

/**
 * @brief QMdiSubWindow that can be detached to become a floating window.
 *
 * Features:
 * - Right-click title bar menu with Detach/Reattach option
 * - Preserves state when transitioning between MDI and floating
 */
class DetachableMdiSubWindow : public QMdiSubWindow {
    Q_OBJECT

private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.detachable_mdi_sub_window");
        return instance;
    }

public:
    explicit DetachableMdiSubWindow(QWidget* parent = nullptr);
    ~DetachableMdiSubWindow() override = default;

    bool isDetached() const { return isDetached_; }

public slots:
    void detach();
    void reattach();

signals:
    void detachedStateChanged(bool detached);

protected:
    void contextMenuEvent(QContextMenuEvent* event) override;
    void closeEvent(QCloseEvent* event) override;

private:
    bool isDetached_;
    QPointer<QMdiArea> savedMdiArea_;
    QPoint savedMdiPosition_;
    QSize savedMdiSize_;
};

}

#endif
