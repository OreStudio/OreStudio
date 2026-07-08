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
#ifndef ORES_QT_QA_VALIDATION_RUNNER_WIDGET_HPP
#define ORES_QT_QA_VALIDATION_RUNNER_WIDGET_HPP

#include "ores.qt/export.hpp"
#include <QList>
#include <QString>
#include <QWidget>
#include <vector>

class QLabel;
class QListWidget;
class QListWidgetItem;
class QToolBar;
class QAction;

namespace ores::qt {

/**
 * @brief One step's in-memory state while a scenario is loaded. Each
 * step is its own org heading under =* Steps= (see the =test_scenario=
 * template) — @p title is that heading's title, @p body its
 * instructional text, rendered read-only in the step detail dialog.
 */
enum class step_status { pending, pass, fail };

struct qa_step final {
    QString title;
    QString body;
    step_status status = step_status::pending;
    QString notes;

    /**
     * @brief Which client instance this step belongs to (e.g. "blue"),
     * for a multi-client scenario — display only (prefixes the step's
     * list entry); the org heading title used to locate/write the
     * step's result is @p title alone, unprefixed.
     */
    QString client;
};

/**
 * @brief The QA Validation Runner: opens a =test_scenario= doc, shows
 * its info and a list of steps (each its own org heading), lets the
 * tester open a step to read its instructions, mark it PASS/FAIL/still
 * pending and write a note, then saves — rewriting the doc's overall
 * =* Results= and each step's =*** Result= child heading in place via
 * =write_scenario_results()= and stamping the compass journal. The
 * scenario's overall outcome is inferred from the step results, not
 * set directly by the tester.
 *
 * A regular widget — hosted in an MDI subwindow (via
 * =DetachableMdiSubWindow=) by the plugin that owns it, not a
 * dock, so the tester can freely move/resize it like every other
 * window while running through a test.
 */
class ORES_QT_API QaValidationRunnerWidget final : public QWidget {
    Q_OBJECT

public:
    explicit QaValidationRunnerWidget(QWidget* parent = nullptr);

public slots:
    /**
     * @brief Parse @p path as a =test_scenario= doc and populate the
     * step list from it. Replaces any previously loaded scenario.
     * @return true on success; emits errorOccurred() and leaves the
     * widget showing its previous (or empty) state on failure.
     */
    bool loadScenario(const QString& path);

    /**
     * @brief Prompt the tester for a =.org= file and load it.
     */
    void promptAndLoadScenario();

    /**
     * @brief Write the current step results back to the loaded
     * scenario doc and stamp the compass journal.
     * @return true on success.
     */
    bool save();

signals:
    void statusMessage(const QString& message);
    void errorOccurred(const QString& message);
    void saved(const QString& path);

private slots:
    void openStepDetail(QListWidgetItem* item);

private:
    void rebuildStepList();
    void updateStepItem(int index);
    void updateOverallStatusLabel();
    void stampJournal(const QString& status);

    QString scenarioPath_;
    QString taskId_;
    QString storyId_;

    std::vector<qa_step> steps_;

    QToolBar* toolBar_;
    QAction* openAction_;
    QAction* saveAction_;
    QLabel* titleLabel_;
    QLabel* targetDialogLabel_;
    QLabel* overallStatusLabel_;
    QListWidget* stepsList_;
};

}

#endif
