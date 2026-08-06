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
#ifndef ORES_QT_SUBJECT_AREA_HISTORY_DIALOG_HPP
#define ORES_QT_SUBJECT_AREA_HISTORY_DIALOG_HPP

#include "ores.dq.api/domain/subject_area.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include <QString>
#include <memory>
#include <vector>

namespace Ui {
class SubjectAreaHistoryDialog;
}

namespace ores::qt {

class SubjectAreaHistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.subject_area_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit SubjectAreaHistoryDialog(const QString& name,
                                      const QString& domain_name,
                                      ClientManager* clientManager,
                                      QWidget* parent = nullptr);
    ~SubjectAreaHistoryDialog() override;

    void loadHistory() override;

    /**
     * @brief Returns the identifier of the subject area.
     */
    [[nodiscard]] QString code() const override {
        return name_;
    }

signals:
    void openVersionRequested(const dq::domain::subject_area& subject_area, int versionNumber);
    void revertVersionRequested(const dq::domain::subject_area& subject_area);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] DiffResult calculateDiffAt(int current_index, int previous_index) const override;
    void displayFullDetails(int index) override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;

private:
    std::unique_ptr<Ui::SubjectAreaHistoryDialog> ui_;
    QString name_;
    QString domain_name_;
    ClientManager* clientManager_;
    std::vector<dq::domain::subject_area> versions_;
};

}

#endif
