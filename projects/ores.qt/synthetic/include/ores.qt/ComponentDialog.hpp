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
#ifndef ORES_QT_COMPONENT_DIALOG_HPP
#define ORES_QT_COMPONENT_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include <QDialog>
#include <QDoubleSpinBox>
#include <QSpinBox>
#include <string>

namespace ores::qt {

/**
 * @brief Modal dialog for creating or editing a GMM component.
 *
 * A component is a gmm_component: one normal distribution (mean, stdev) with a
 * mixture weight in the price model of an FX pair. The dialog persists the
 * component on Save (off the UI thread) and only calls accept() once the
 * server confirms success.
 */
class ComponentDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.component_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a dialog for a new component under @p parentFxId.
     *
     * @param nextIndex the default component index (next free position).
     */
    ComponentDialog(ClientManager* cm,
                    const QString& username,
                    const boost::uuids::uuid& parentFxId,
                    int nextIndex,
                    QWidget* parent = nullptr);

    /**
     * @brief Construct a dialog editing an existing component.
     */
    ComponentDialog(ClientManager* cm,
                    const QString& username,
                    const synthetic::domain::gmm_component& existing,
                    QWidget* parent = nullptr);

    ~ComponentDialog() override = default;

private slots:
    void onSave();

private:
    void buildUi();

    ClientManager* clientManager_;
    QString username_;
    bool isNew_;
    synthetic::domain::gmm_component component_;

    QSpinBox* indexSpin_;
    QDoubleSpinBox* meanSpin_;
    QDoubleSpinBox* stdevSpin_;
    QDoubleSpinBox* weightSpin_;
};

}

#endif
